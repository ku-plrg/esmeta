package esmeta.tracer

import esmeta.*
import esmeta.error.NotSupported
import esmeta.es.*
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.tracer.util.*
import esmeta.util.{ConcurrentPolicy as CP}
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

import io.circe.syntax.*

import scala.concurrent.TimeoutException
import scala.concurrent.duration.*

import java.io.PrintStream
import java.nio.file.*
import java.util.concurrent.BlockingQueue

class TraceDumper(
  targetTests: List[Test],
  removed: Iterable[(Test, List[String])],
  isTest262: Boolean,
  // paths: Option[List[String]],
  log: Boolean = true,
  timeLimit: Option[Int] = None,
  targetTracer: Set[TargetKind] = Set(),
  handleNotSupportedAsFail: Boolean = false,
  useStdout: Boolean = true,
)(using
  test262: Test262,
  workdir: WorkDir,
) { jalangi =>

  lazy val targetNodeprof: Boolean = targetTracer.contains(TargetKind.NodeProf)
  lazy val targetDynaJS: Boolean = targetTracer.contains(TargetKind.DynaJS)
  lazy val targetJalangi: Boolean = targetTracer.contains(TargetKind.Jalangi)

  // pre-jobs
  lazy val multiple = targetTests.length > 1
  def printlnIfSingle(s: => String): Unit =
    if (useStdout && !multiple) println(s)

  def test: Summary = {
    mkdir(workdir.path)

    executeCmdNonZero(
      s"npm run build",
      dir = CHECK_SCRIPT_DIR,
    ) match
      case (a, b, c) =>
        printlnIfSingle(s"Checkscript build output[${a}]:\n${b}\n${c}")

    val runner = {

      new Test262Runner(
        msg = "Jalangi Test262 Test",
        targets = targetTests,
        notSupported = removed,
        concurrent = CP.Fixed(8),
        showProgressBar = useStdout,
        verbose = useStdout,
        detail = useStdout,
      ) {

        lazy val elemize: (Test, Int) => String = (test, _) => {
          test.absPath.replace("/", "_")
        }

        def runTest(test: Test): Unit = {

          val filename = elemize(test, -1)

          val tmpFilePath = s"${workdir.path}/$filename"
          val (ast, code) =
            if isTest262 then test262.loadTest(test.absPath)
            else (test262.cfg.scriptParser.fromFileWithCode(test.absPath))
          dumpFile(
            "",
            code,
            tmpFilePath,
            silent = !(useStdout && !multiple),
          )

          printlnIfSingle(s"Running Jalangi for ${tmpFilePath}")

          val isES5 = Aux.checkES5(tmpFilePath)
          val isValid = Aux.checkRequirements(tmpFilePath) match
            case (false, str, err) => false
            case _                 => true
          val isAvailableForJalangi = isES5 && isValid
          val isAvailableForDynaJS = isValid
          val isAvailableForNodeProf = isValid

          Aux.runNodeJs(tmpFilePath) match
            case false => throw NotSupported("Does not pass Node.js")
            case true  => ()

          // keep this to check if ESMeta-not-supported features exist
          Aux.runESMetaInterp(code, ast, timeLimit)

          if (targetJalangi) {
            if (isAvailableForJalangi) {
              val (jalangiOutput, jalangiErr) = Aux.runJalangi(tmpFilePath)
              dumpFile(
                "", // name placeholder to set silent = !useStdout
                jalangiOutput,
                s"${workdir.path}/${elemize(test, -1)}.jalangi-trace",
                silent = !(useStdout && !multiple),
              )
            } else {
              printlnIfSingle(
                s"Skipping Jalangi for ${tmpFilePath} since it is not supported by Jalangi (maybe due to ES6+ features?)",
              )
            }
          }

          if (targetNodeprof) {
            val (npOutput, npErr) = Aux.runNodeProf(
              analysisPath = ANALYSIS_FILE_PATH_COPIED_TO_TMP,
              testPath = tmpFilePath,
            )
            dumpFile(
              "", // name placeholder to set silent = !useStdout
              npOutput,
              s"${workdir.path}/${elemize(test, -1)}.nodeprof-trace",
              silent = !(useStdout && !multiple),
            )
          }

          if (targetDynaJS) {
            if (isAvailableForDynaJS) {
              val (dynaOutput, dynaErr) = Aux.runDynaJS(tmpFilePath)
              dumpFile(
                "", // name placeholder to set silent = !useStdout
                dynaOutput,
                s"${workdir.path}/${elemize(test, -1)}.dynajs-trace",
                silent = !(useStdout && !multiple),
              )
            } else {
              printlnIfSingle(
                s"Skipping DynaJS for ${tmpFilePath} since it is not supported by DynaJS (maybe due to ES6+ features?)",
              )
            }
          }

          val kind =
            (if (targetJalangi && isAvailableForJalangi) then
               Set(TargetKind.Jalangi)
             else Set()) ++
            (if targetDynaJS && isAvailableForDynaJS then Set(TargetKind.DynaJS)
             else Set()) ++
            (if targetNodeprof && isAvailableForNodeProf then
               Set(TargetKind.NodeProf)
             else Set())

          if (kind.isEmpty) {
            throw NotSupported(
              "The test may include new features not supported by the target tracer(s)",
            )
          }

          dumpFile(
            "", // name placeholder to set silent = !useStdout
            {
              import esmeta.tracer.util.JsonProtocol.given
              CachedTrace(
                originalAliasName = "TODO",
                absPath = s"${workdir.path}/${elemize(test, -1)}",
                originalAbsPath = test.absPath,
                kind = kind,
              ).asJson
            },
            s"${workdir.path}/${elemize(test, -1)}.metadata",
            silent = !(useStdout && !multiple),
          )

        }

        override def errorHandler(
          error: Throwable,
          summary: Summary,
          name: String,
          test: Test,
        ): Unit =
          if (multiple) error match
            case NotSupported(reasons) =>
              if (handleNotSupportedAsFail) {
                println(s"[FAIL   ] $name: ${getMessage((error))}")
                summary.fail.add(name, getMessage((error)))
              } else {
                summary.notSupported.add(name, reasons)
              }
            case _: TimeoutException =>
              println(s"[TIMEOUT] $name")
              summary.timeout.add(name)
            case e: Throwable =>
              println(s"[FAIL   ] $name: ${getMessage((e))}")
              summary.fail.add(name, getMessage((e)))
          else {
            (error, handleNotSupportedAsFail) match
              case (NotSupported(reasons), true) =>
                raise(reasons.mkString(", "))
              case _ => throw error
          }

        override def postJob: Summary = {
          val summary = super.postJob
          val logDir = s"${LOG_DIR}/trace-dumper"

          // logging after tests
          if (log)
            summary.dumpTo(logDir, silent = !useStdout)
            dumpFile(
              s"Jalangi test summary",
              summary.toString,
              s"$logDir/summary",
              silent = !(useStdout && !multiple),
            )

          // if exists(logDir) then
          // createSymLink(symlink, logDir, overwrite = true)
          // logPW.close()
          summary
        }
      }
    }

    runner.result
  }

  val JALANGI_HOME: String = {
    sys.env.getOrElse(
      "JALANGI_HOME",
      throw new RuntimeException("JALANGI_HOME not set"),
    )
  }
  val CHECK_SCRIPT_DIR: String = { s"${BASE_DIR}/checkscript" }
  val ANALYSIS_FILE_PATH: String = {
    val path = java.nio.file.Paths.get(BASE_DIR, "analysis.compare.js")
    if (!java.nio.file.Files.exists(path))
      throw new java.io.FileNotFoundException(s"${path} not found")
    path.toString
  }
  val ANALYSIS_DYNAJS_FILE_PATH: String = {
    val path = java.nio.file.Paths.get(BASE_DIR, "analysis.dyna.js")
    if (!java.nio.file.Files.exists(path))
      throw new java.io.FileNotFoundException(s"${path} not found")
    path.toString
  }
  val DYNAJS_HOME: String = {
    sys.env.getOrElse(
      "DYNAJS_HOME",
      throw new RuntimeException("DYNAJS_HOME not set"),
    )
  }

  lazy val ANALYSIS_FILE_PATH_COPIED_TO_TMP: String = {
    val destPath = s"${workdir.path}/analysis.js"
    copyFile(ANALYSIS_FILE_PATH, destPath)
    destPath
  }

  object Aux {

    /** Check syntax of a test file using acorn and some requirements. It should
      * be a valid JS syntax, not include `this`, `use strict`, etc.
      */
    def checkRequirements(filename: String): (Boolean, String, String) = {
      val CHECK_VALIDITY_FOR_JALANGI: String =
        s"$BASE_DIR/checkscript/dist/check-require.js"
      val CMD = s"node $CHECK_VALIDITY_FOR_JALANGI $filename"
      val (code, o, e) = executeCmdNonZero(CMD)
      (code == 0, o, e)
    }

    /** Check syntax of a test file using. It should be a valid ES5 syntax */
    def checkES5(filename: String): Boolean = {
      val CHECK_VALIDITY_ES5: String =
        s"$BASE_DIR/checkscript/dist/check-es5.js"
      val CMD = s"node $CHECK_VALIDITY_ES5 $filename"
      val (code, _, _) = executeCmdNonZero(CMD)
      code == 0
    }

    lazy val BOOTSTRAP_NODE_JS: String = s"$BASE_DIR/bootstrap-node.js"
    lazy val BOOTSTRAP_NODE_JS_COPIED: String = { ??? }

    def runNodeJs(
      testpath: String,
    ): Boolean = {
      val CMD = s"node --require $BOOTSTRAP_NODE_JS $testpath"
      val (code, o, e) = executeCmdNonZero(CMD)
      printlnIfSingle(s"Node.js output:\n${o}\n${e}")
      code == 0
      // if it doesn't pass node.js, it is not worth to run Jalangi
    }

    def runGraalNodeJs(
      testpath: String,
    ): Boolean = {
      ???
      // val (exitcode, _, _) = executeCmdNonZero(
      //   s"""
      //   $ALIAS_DECL;
      //   nodeprof node --require $BOOTSTRAP_NODE_JS_COPIED $testpath""",
      // )
      // exitcode == 0
      // // if it doesn't pass node.js, it is not worth to run Jalangi
    }

    /** @param wd
      *   absolute working directory
      * @param analysisPath
      *   absolute analysis file path
      * @param testPath
      *   absolute test file path
      * @return
      *   Jalangi output
      */
    def runNodeProf(
      analysisPath: String,
      testPath: String,
    ): (String, String) = {

      import java.nio.file.{Path, Paths, Files}

      // TODO make sure mx jalangi's boot.js has same content as BOOTSTRAP_NODE_JS

      val base = Paths.get(workdir.path)

      val testPathRelative = base.relativize {
        Paths.get(testPath).toAbsolutePath.normalize
      }.toString

      val analysisPathRelative = base.relativize {
        Paths.get(analysisPath).toAbsolutePath.normalize
      }.toString

      val cmd =
        // TODO publish docker image ku-plrg? or build locally at the pre-job?
        s"docker run --rm -v ${workdir.path}:/works/nodeprof.js/input ghcr.io/stonechoe/nodeprof jalangi --analysis $analysisPathRelative $testPathRelative"
      // s"mx jalangi --analysis $analysisPath $testPath"
      val (str, err) =
        try {
          // printlnIfSingle(s"Executing Jalangi command: $cmd")
          val (s, err) =
            executeCmdTimeout(cmd, duration = 60.seconds, dir = workdir.path)
              .getOrElse(
                throw java.util.concurrent.TimeoutException(
                  "NodeProf timed out, maybe because of ES6+ features?",
                ),
              )
          if (s.startsWith("Failed to instrument"))
            throw NotSupported(
              "The test may include new features not supported by NodeProf",
            )
          (s, err)
          // maybe test includes new features not supported by Jalangi
        } catch {
          case e: Throwable =>
            printlnIfSingle(
              s"Error in NodeProf Test262 Test: ${e.getStackTrace()}",
            )
            throw e;
        }
      (str, err)
    }

    def runESMetaInterp(
      code: String,
      ast: Ast,
      timeLimit: Option[Int],
    )(using test262: Test262): Unit = {
      NotSupportedChecker(
        test262.cfg.init.from(code, ast),
        timeLimit = timeLimit,
      ).result
    }

    def runDynaJS(tmpFilePath: String): (String, String) = {
      try {
        val (s, err) =
          executeCmdTimeout(
            s"node --require ${BOOTSTRAP_NODE_JS} ${DYNAJS_HOME}/dist/index.js analyze -a $ANALYSIS_DYNAJS_FILE_PATH $tmpFilePath",
            duration = 60.seconds,
          ).getOrElse(
            throw java.util.concurrent.TimeoutException(
              "DynaJS timed out, maybe because of ES6+ features?",
            ),
          )
        if (s.startsWith("Failed to instrument"))
          throw NotSupported(
            "The test may include new features not supported by DynaJS",
          )
        (s, err)
        // maybe test includes new features not supported by DynaJS
      } catch {
        case e: Throwable =>
          printlnIfSingle(
            s"Error in DynaJS Test262 Test: ${e.getStackTrace()}",
          )
          throw e;
      }
    }

    def runJalangi(tmpFilePath: String): (String, String) = {
      val (str, err) =
        try {
          // printlnIfSingle(s"Executing Jalangi command: $jalangiCmd")
          val (s, err) =
            executeCmdTimeout(
              s"node --require ${BOOTSTRAP_NODE_JS} ${JALANGI_HOME}/src/js/commands/jalangi.js --inlineIID --inlineSource --analysis $ANALYSIS_FILE_PATH $tmpFilePath",
              duration = 60.seconds,
            ).getOrElse(
              throw java.util.concurrent.TimeoutException(
                "Jalangi timed out, maybe because of ES6+ features?",
              ),
            )
          if (s.startsWith("Failed to instrument"))
            throw NotSupported(
              "The test may include new features not supported by Jalangi",
            )
          (s, err)
          // maybe test includes new features not supported by Jalangi
        } catch {
          case e: Throwable =>
            printlnIfSingle(
              s"Error in Jalangi Test262 Test: ${e.getStackTrace()}",
            )
            throw e;
        }
      (str, err)
    }

    lazy val ALIAS_DECL =
      "alias nodeprof='docker run --rm -v $(pwd):/works/nodeprof.js/input ghcr.io/stonechoe/nodeprof:latest'"
  }
}

object TraceDumper {
  def clean(using wd: WorkDir): Unit = { rmdir(wd.path) }

  def computeTarget(
    targets: Option[List[String]],
  )(using test262: Test262) = {
    // adhoc patch to run non-test262, plain JS files
    targets match
      case None =>
        lazy val tests: List[Test] =
          test262.getTests(targets, features = None)
        test262.testFilter(tests, test262.withYet) -> true
      case Some(value) =>
        val absPaths = value.map { p =>
          val path = Paths.get(p)
          path.toAbsolutePath().normalize().toString()
        }
        val test262path = Paths
          .get(TEST262_DIR)
          .toAbsolutePath()
          .normalize()
          .toString()
        val isTest262 = absPaths.forall(_.startsWith(test262path))
        if (isTest262) {
          lazy val tests: List[Test] =
            test262.getTests(targets, features = None)
          test262.testFilter(tests, test262.withYet) -> true
        } else {
          val tests = absPaths.map { p =>
            Test(
              path = p,
              negative = None,
              flags = List(),
              includes = List(),
              locales = List(),
              features = List(),
              es5 = true, // ???
            )
          }
          (tests, Nil) -> false
        }
  }
}
