package fi.aalto.cs.replace

import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiComment, PsiElement, PsiErrorElement, PsiWhiteSpace}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScExtension, ScFunction, ScTypeAlias}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.project.{ProjectContext, ScalaFeatures}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{
  ScEnum,
  ScGiven,
  ScGivenDefinition,
  ScObject,
  ScTypeDefinition
}

import scala.jdk.CollectionConverters.*

/** Splits a multiline Scala 3 paste into the groups that are each submitted to the REPL as one
  * compilation unit. Statements in one unit cannot redefine one another, and statements in separate
  * units cannot see each other, so runs of adjacent definitions stay in one group and everything
  * else is submitted alone. A definition that rebinds a name already bound in its group starts a
  * new group. The result is line-by-line semantics, under which a forward reference that a
  * whole-paste unit allowed now fails.
  */
object Scala3StatementSplitter:

  /** One submission group, with the blank lines its temporary file needs so that compiler
    * diagnostics keep their paste-relative line numbers.
    */
  final case class Chunk(source: String, lineOffset: Int)

  /** Splits `source` into submission groups, each reconstructed from the original text with line
    * separators normalized to `\n` and carrying its zero-based line offset in the paste. Returns no
    * chunks when the source does not parse cleanly, and the caller then sends the paste verbatim.
    * `features` must be the module's own, or newer syntax silently forces that fallback.
    */
  def splitWithLineOffsets(
      source: String,
      features: ScalaFeatures,
      project: Project
  ): List[Chunk] =
    // Parsing and substringing the same normalized text keeps the offsets exact for any input.
    val normalized = StringUtil.convertLineSeparators(source)
    ReadAction.computeBlocking { () =>
      // shouldTrimText = false, or leading blanks shift every element offset against `normalized`.
      val file = ScalaPsiElementFactory.createScalaFileFromText(
        normalized,
        features,
        shouldTrimText = false
      )(using ProjectContext(project))

      // A def, so that the comment traversal is skipped whenever the error check already decided.
      def hasUnterminatedBlockComment = PsiTreeUtil
        .findChildrenOfType(file, classOf[PsiComment])
        .asScala
        .exists(comment => hasUnclosedBlockComment(comment.getText))

      if PsiTreeUtil.findChildOfType(file, classOf[PsiErrorElement]) != null ||
        hasUnterminatedBlockComment
      then Nil
      else
        // Statement separators are file-level leaves, and keeping them would break definition runs.
        val statements = file.getChildren.toList.filterNot(element =>
          element.isInstanceOf[PsiWhiteSpace] || element.isInstanceOf[PsiComment] ||
            element.getNode.getElementType == ScalaTokenTypes.tSEMICOLON
        )
        val groups = statements
          .foldLeft(List.empty[Group]) { (acc, statement) =>
            val names = boundNames(statement)
            acc match
              // Only definitions bind names, so a group that bound any is a definition run.
              case current :: rest
                  if names.nonEmpty && current.bound.nonEmpty && !names.exists(current.bound) =>
                current.extendTo(statement, names) :: rest
              case _ =>
                Group.starting(statement, names) :: acc
          }
          .reverse
        groups.map { group =>
          // Substringing between element bounds reconstructs the group exactly. The start extends
          // to the head's line start, so a paste copied from inside a class body stays indented.
          val start = lineStartBefore(normalized, group.start)
          // A paste is small, so counting newlines from the top per group is cheap and plain.
          Chunk(normalized.substring(start, group.end), normalized.take(start).count(_ == '\n'))
        }
    }

  /** A group under construction: the text span it covers and the names it binds so far. */
  private final case class Group(start: Int, end: Int, bound: Set[String]):
    def extendTo(statement: PsiElement, names: Set[String]): Group =
      copy(end = statement.getTextRange.getEndOffset, bound = bound ++ names)

  private object Group:
    def starting(statement: PsiElement, names: Set[String]): Group =
      val range = statement.getTextRange
      Group(range.getStartOffset, range.getEndOffset, names)

  /** The start of `offset`'s line when only blanks precede it there, and `offset` otherwise, so a
    * statement sharing a line with the previous one drags no text along.
    */
  private def lineStartBefore(text: String, offset: Int): Int =
    val lineStart = text.lastIndexOf('\n', offset - 1) + 1
    if (lineStart until offset).forall(i => text(i) == ' ' || text(i) == '\t') then lineStart
    else offset

  /** The names a definition binds, qualified by namespace so a class and its companion object do
    * not collide. Extension keys also retain the receiver's source text. The cases are ordered most
    * specific first and every early case matches a later one too, so none may move below another.
    */
  private def boundNames(element: PsiElement): Set[String] = element match
    // Both spellings of a given bind the same term namespace, so they must collide and split.
    case givenDefinition: ScGiven =>
      // Alias givens are functions in PSI, and structural givens desugar to an object or to a
      // class plus a function. Mirroring those namespaces also catches a generated given name.
      givenDefinition match
        case function: ScFunction => functionNames(function)
        case definition: ScGivenDefinition if !definition.isObject =>
          callableNames(
            definition.name,
            definition.parameters.headOption.flatMap(_.typeElement),
            definition.parameters.isEmpty
          ) + s"type:${definition.name}"
        case _ => Set(s"singleton:${givenDefinition.name}")
    case function: ScFunction => functionNames(function)
    case extension: ScExtension =>
      val target = extension.targetTypeElement.map(receiverText).getOrElse("_")
      extension.extensionMethods.map(method => s"callable:$target#${method.name}").toSet
    case obj: ScObject => Set(s"singleton:${obj.name}")
    case enumDefinition: ScEnum =>
      Set(s"type:${enumDefinition.name}", s"synthetic-companion:${enumDefinition.name}")
    case typeDefinition: ScTypeDefinition =>
      Set(s"type:${typeDefinition.name}") ++
        Option.when(typeDefinition.isCase)(s"synthetic-companion:${typeDefinition.name}")
    case typeAlias: ScTypeAlias => Set(s"type:${typeAlias.name}")
    // Imports, exports, vals and expressions bind no names here, so each is submitted alone.
    case _ => Set.empty

  /** Receivers are keyed by source text, so one spelling of a type always collides with itself and
    * two spellings never do. Two receivers that erase alike but read differently, such as
    * `List[Int]` and `List[String]`, therefore share a unit and fail to compile together, an
    * accepted trade for not modelling erasure here.
    */
  private def receiverText(typeElement: ScTypeElement): String =
    typeElement.getText.filterNot(_.isWhitespace)

  /** Keyed by name alone, which can split adjacent overloads but needs no model of signatures. */
  private def functionNames(function: ScFunction): Set[String] =
    callableNames(
      function.name,
      function.parameters.headOption.flatMap(_.typeElement),
      function.parameters.isEmpty
    )

  private def callableNames(
      name: String,
      firstParameterType: Option[ScTypeElement],
      parameterless: Boolean
  ): Set[String] =
    val receiverKey =
      firstParameterType.map(typeElement => s"callable:${receiverText(typeElement)}#$name")
    Set(s"function:$name") ++ receiverKey ++
      (if parameterless then Set(s"singleton:$name", s"synthetic-companion:$name") else Set.empty)

  /** Scala block comments nest, so a final closing delimiter may leave the outer one open. PSI does
    * not always report that as an error.
    */
  private def hasUnclosedBlockComment(text: String): Boolean =
    if !text.startsWith("/*") then false
    else
      var depth = 0
      var index = 0
      while index + 1 < text.length do
        if text.charAt(index) == '/' && text.charAt(index + 1) == '*' then
          depth += 1
          index += 2
        else if text.charAt(index) == '*' && text.charAt(index + 1) == '/' then
          depth -= 1
          index += 2
        else index += 1
      depth > 0
