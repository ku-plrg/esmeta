package esmeta.phase

import esmeta.{CommandConfig, TEST_MODE, TEST262_DIR, LOG_DIR}
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
import esmeta.Command
import esmeta.CmdBuildCFG

/** `test262-dynajs` phase */
case object Test262DynaJS extends Phase[CFG, Unit] {

  self =>
  val name = "test262-dynajs"
  val help = "..."

  val workdir = WorkDir(s"$LOG_DIR/$name-tmp")

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
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

    lazy val ((targetTests, removed), isTest262) =
      DynaJSTest262Runner.computeTarget(targets)(using test262)

    DynaJSTest262Runner(
      targetTests,
      removed,
      isTest262,
      timeLimit = Some(60000),
      comparePartial = config.comparePartial,
    )(using test262, workdir).test
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
      "compare-partial",
      BoolOption(_.comparePartial = _),
      "compare partial results (e.g., only final states) instead of full results (default: true).",
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
    var comparePartial: Boolean = true,
  )
}
