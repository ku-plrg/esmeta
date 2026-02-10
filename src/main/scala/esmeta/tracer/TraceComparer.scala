package esmeta.tracer

import esmeta.*
import esmeta.cfg.CFG
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.error.NotSupported
import esmeta.es.*
import esmeta.util.{ConcurrentPolicy as CP, *}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*
import java.io.PrintStream
import java.nio.file.*

class TraceComparer(
  cfg: CFG,
  metadata: CachedTraceMetadata,
  log: Boolean = true,
  timeLimit: Option[FiniteDuration] = None,
  targetTracer: Set[TargetKind] = Set(),
  useStdout: Boolean = true,
) { self =>

  given CFG = cfg

  lazy val elemize: (CachedTrace, Int) => String = (t, _) =>
    s"${t.kind} ${t.originalAbsPath}"

  lazy val multiple = metadata.traces.length > 1
  def printlnIfSingle(s: => String): Unit =
    if (useStdout && !multiple) println(s)

  // summary
  lazy val summary = Summary()

  def test: Summary = {

    val runner = {
      new JobRunner[CachedTrace, Summary](
        // msg = "Jalangi Test262 Test",
        targets = metadata.traces,
        // notSupported = removed,
        concurrent = CP.Fixed(8),
        showProgressBar = useStdout,
      ) {
        // lazy val elemize: (Test, Int) => String = (t, _) => t.relName

        def job(t: CachedTrace, idx: Int): Unit = {
          val name = elemize(t, idx)
          getError {
            timeLimit match
              case None            => runTest(t)
              case Some(timeLimit) => timeout(runTest(t), timeLimit)
            summary.pass.add(name)
          }.map(errorHandler(_, summary, name, t))
        }

        def runTest(t: CachedTrace): Unit = {

// val tmpFilePath =
//   if isTest262 then
//     s"$TEMP_FILE_BASE/${test.relName.replace("/", "_")}"
//   else s"$TEMP_FILE_BASE/${test.absPath.replace("/", "_")}"
// val (ast, code) =
//   if isTest262 then test262.loadTest(test.absPath)
//   else (test262.cfg.scriptParser.fromFileWithCode(test.absPath))
// dumpFile(code, tmpFilePath)

          // TODO this is "bad" way to get test262, since they have so many harness
          val (ast, code) = cfg.scriptParser.fromFileWithCode(t.absPath)

          val esmetaTrace = Aux.runESMetaInterp(code, ast, Some(60000))
          val esmetaTree = TraceTree.apply(esmetaTrace.traces)

          printlnIfSingle(t.toString)

          def checkKind(kind: TargetKind): Boolean = {
            lazy val requested = targetTracer.contains(kind)
            lazy val available = t.kind.contains(kind)
            if (requested && !available) {
              raise(
                s"Trace for target kind ${kind} is requested, " +
                s"but not found for test: ${t.absPath}",
              )
            }
            requested && available
          }

          val doJalangi = checkKind(TargetKind.Jalangi)
          val doNodeProf = checkKind(TargetKind.NodeProf)
          val doDynaJS = checkKind(TargetKind.DynaJS)

          if (doJalangi) {
            val jalangiPath = s"${t.absPath}.jalangi-trace"
            testTrace(esmetaTree, jalangiPath, TargetKind.Jalangi)
          }
          if (doNodeProf) {
            val nodeprofPath = s"${t.absPath}.nodeprof-trace"
            testTrace(esmetaTree, nodeprofPath, TargetKind.NodeProf)
          }
          if (doDynaJS) {
            val dynajsPath = s"${t.absPath}.dynajs-trace"
            testTrace(esmetaTree, dynajsPath, TargetKind.DynaJS)
          }

        }

        def testTrace(
          esmetaTree: TraceTree,
          absPath: String,
          kind: TargetKind,
        ): Unit = {
          val tracelog = readFile(absPath).split("\n")
          val tree = TraceTree.apply(tracelog.map(Trace.from).toList)

          val rawDiff = esmetaTree.diff(tree)
          printlnIfSingle("=========== Raw Diff Trace ==========")
          printlnIfSingle(rawDiff.prettyprint)
          printlnIfSingle("=====================================")
          val ignoredDiff = Ignore.apply(rawDiff)(using kind)
          printlnIfSingle("============= Diff Trace ============")
          printlnIfSingle(ignoredDiff.prettyprint)
          printlnIfSingle("=====================================")

          lazy val pass = ignoredDiff.isAllSame

          if (pass)
            printlnIfSingle(
              s"${Console.GREEN}Test ${absPath} passed for ${kind}.${Console.RESET}",
            )
          else
            printlnIfSingle(
              s"${Console.RED}Test ${absPath} failed for ${kind}.${Console.RESET}",
            )
            throw new Exception(s"Test failed for ${kind}")
        }

        def errorHandler(
          error: Throwable,
          summary: Summary,
          name: String,
          t: CachedTrace,
        ): Unit =
          if (multiple) error match
            case NotSupported(reasons) =>
              summary.notSupported.add(name, reasons)
            case _: TimeoutException =>
              println(s"[TIMEOUT] $name")
              summary.timeout.add(name)
            case e: Throwable =>
              // if (log)
              //   logPW.println(s"[FAIL   ] $name")
              //   logPW.println(e.getStackTrace.mkString(LINE_SEP))
              //   logPW.flush

              // synchronized {
              //   println(s"[FAIL   ] $name: ${getMessage((e))}")
              // }
              summary.fail.add(name, getMessage((e)))
          else throw error

        def postJob: Summary = {
          val logDir = s"${LOG_DIR}/trace-comparer"

          // logging after tests
          if (log) {
            summary.dumpTo(logDir, silent = !useStdout)
            dumpFile(
              s"Jalangi test summary",
              summary.toString,
              s"$logDir/summary",
              silent = !useStdout,
            )
          }

          // if exists(logDir) then
          // createSymLink(symlink, logDir, overwrite = true)
          // logPW.close()
          summary
        }

        override def barPostfix =
          (if (summary.total == summary.passCount) ""
           else s" - ${summary.simpleString}") +
          s" [${summary.time.simpleString}]"

        override def tick: Unit = summary.time = elapsedTime
      }

    }

    runner.result
  }

  object Aux {

    def runESMetaInterp(
      code: String,
      ast: Ast,
      timeLimit: Option[Int],
    )(using cfg: CFG): TraceLog = {
      TracingInterpreter(
        cfg.init.from(code, ast),
        timeLimit = timeLimit,
        analysis = new Analysis(),
      ).trace
    }

  }
}
