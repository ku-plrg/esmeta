package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.test262.*

/** `test262test` phase */
case object Test262Test extends Phase[CFG, Option[String]] {
  val name = "test262test"
  val help = "test a Test262 program with harness files."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Option[String] = try {
    // TODO support directory
    val filename = getFirstFilename(cmdConfig, this.name)
    val initSt = Initialize.fromFile(cfg, filename, test262 = true)
    val st = Interpreter(initSt, log = config.log, timeLimit = config.timeLimit)

    // check final state
    st(GLOBAL_RESULT) match
      case Undef => None
      case v     => Some(s"return not undefined: $v")
  } catch { case e: Throwable => Some(s"unexpected error: $e") }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
  )
}
