# REPLace

REPLace is an IntelliJ IDEA plugin that replaces the Scala plugin's REPL console. It is built for Aalto University's
introductory programming courses.

## What it changes

REPLace overrides the Scala plugin's `Scala.RunConsole` and `ScalaConsole.Execute` actions.

- Ctrl+Shift+D starts a REPL for the module of the open file, or of the project tree selection.
- The console opens with a welcome message listing the auto-imported packages and the shortcuts for running code,
  browsing history, and restarting the REPL.
- If the module code is edited while a REPL is open, a banner reminds the student to restart it for changes to take
  effect.

## Per-project configuration

Both files are optional.

- `.idea/.repl-arguments` holds extra Scala compiler options for the REPL command line. `scalac -help` lists the
  standard options, with more behind `-X`, `-Y`, `-V` and `-W`. For example:

```
-new-syntax -feature -deprecation
```

- `<module>/.repl-commands` holds commands to run when the REPL starts. The file is hidden from the file tree. For
  example:

```scala
import o1.*
import o1.goodstuff.*
import o1.goodstuff.gui.*
```

`.idea/aplus_project.xml` marks a project as an A+ course project, which enables the change banner and the
course-specific welcome message.

## Building

```
./gradlew build     # compiles, checks formatting, runs the tests
./gradlew runIde    # sandbox IDE with the plugin installed
```

The sandbox also installs the A+ Courses plugin and Additional Scala Inspections.

Sources are formatted with scalafmt through Spotless. Run `./gradlew spotlessApply` to format the code.

## License

Apache 2.0. See [LICENSE](LICENSE).
