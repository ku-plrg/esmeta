package esmeta.phase

import esmeta.{CommandConfig, TEST_MODE, TEST262_DIR}
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.Coverage
import esmeta.test262.{*, given}
import esmeta.tracer.*
import esmeta.test262.util.{TestFilter, Summary}
import java.io.File
import java.nio.file.Paths
import java.util.concurrent.{TimeoutException}

/** `collect-trace` phase */
case object CollectTrace extends Phase[CFG, (CFG, CachedTraceMetadata)] {
  val name = "collect-trace"
  val help = "collect execution trace for given input file."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): (CFG, CachedTraceMetadata) = {
    // set test mode
    TEST_MODE = true

    val version = Test262.getVersion(config.test262Target)
    val test262 = Test262(version, cfg, config.withYet)
    val targets =
      if (cmdConfig.targets.isEmpty) None
      else Some(cmdConfig.targets)

    if (config.timeLimit.isDefined && config.concurrent == CP.Auto)
      raise(
        "Turing on both time limit option (-test262-test:timeout and " +
        "the concurrent mode (-test262-test:concurrent) with " +
        "automatic thread number is not allowed.",
      )

    if (config.fromCache && !config.tracer.nonEmpty) {
      warn(
        "`tracer` option has no effect when using `from-cache`.",
      )
    }

    if (config.tracer.isEmpty && !config.fromCache) {
      raise(
        "No target kind is specified for tracing. No traces will be collected. To specify target kinds, use -collect-trace:tracer=<kind>.",
      )
    }

    if (config.clean) {
      TraceRestore.clean(using TRACER_WORKDIR)
      warn("Cleaned the tracer working directory.")
    } else if (!config.fromCache) {
      warn(
        "Existing cache in the tracer working directory will be used - this might affect result. To clean, use -collect-trace:clean",
      )
    }

    lazy val ((targetTests, removed), isTest262) =
      TraceDumper.computeTarget(targets)(using test262)

    if (!config.fromCache) {
      TraceDumper(
        targetTests,
        removed,
        isTest262,
        timeLimit = Some(60000),
        targetTracer = config.tracer,
      )(using test262, TRACER_WORKDIR).test
    }

    val restore = TraceRestore
      .restore(targetTests.map(t => t.absPath))(using TRACER_WORKDIR)

    val restoredAbs = restore.traces.map(t => t.originalAbsPath).toSet
    val missingAbs = targetTests.map(t => t.absPath).toSet -- restoredAbs
    if (missingAbs.nonEmpty) {
      warn(
        s"${missingAbs.size} traces not found. This might be due to (1) skipped nodeprof targets, (2) timeouts or errors during trace collection, (3) typo from target pathname, or (4) using from-cache option without existing cache files.",
      )
    }

    cfg -> restore
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "test262-target",
      StrOption((c, s) => c.test262Target = Some(s)),
      "set the target git version of Test262 (default: current version).",
    ),
    (
      "progress",
      BoolOption(_.progress = _),
      "show progress bar.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "with-yet",
      BoolOption(_.withYet = _),
      "test with currently ignored tests because of unknown issues.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "turn on logging mode with detailed information.",
    ),
    (
      "concurrent",
      NumOption((c, k) =>
        c.concurrent =
          if (k <= 0) then CP.Auto else if (k == 1) CP.Single else CP.Fixed(k),
      ),
      "set the number of thread to use concurrently (default: no concurrent)." +
      " If number <= 0, use automatically determined number of threads.",
    ),
    (
      "from-cache",
      BoolOption((c, b) => c.fromCache = b),
      "collect traces from cached trace files instead of running tests.",
    ),
    (
      "clean",
      BoolOption((c, b) => c.clean = b),
      "clean the tracer working directory before collecting traces.",
    ),
    (
      "tracer",
      StrOption((c, s) => {
        val kind = TargetKind.from(s) match
          case Some(value) => value
          case None        => raise(s"Unknown target kind: $s")

        if (c.tracer contains kind) {
          warn(s"Duplicate target kind for 'tracer' option: $s")
        }

        c.tracer += kind
      }),
      "show warning message regarding Test262 tracing.",
    ),
  )
  case class Config(
    var test262Target: Option[String] = None,
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var withYet: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var concurrent: CP = CP.Single,
    //
    var fromCache: Boolean = false,
    var clean: Boolean = false,
    var tracer: Set[TargetKind] = Set(),
  )
}
