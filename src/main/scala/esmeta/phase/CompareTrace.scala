package esmeta.phase

import esmeta.{CommandConfig, TEST_MODE}
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
import scala.concurrent.duration.*
import java.io.File
import java.util.concurrent.TimeoutException

/** `compare-trace` phase */
case object CompareTrace extends Phase[(CFG, CachedTraceMetadata), Unit] {
  val name = "compare-trace"
  val help = "compare execution trace with expected trace for given input file."
  def apply(
    input: (CFG, CachedTraceMetadata),
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val (cfg, cachedTraceMetadata) = input

    TraceComparer(
      cfg,
      cachedTraceMetadata,
      timeLimit = Some(60.seconds),
    ).test

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
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
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
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
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
    var detail: Boolean = false,
    var concurrent: CP = CP.Single,
    //
    var tracer: Set[TargetKind] = Set(),
  )
}
