package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.dump.debugger.*
import esmeta.dump.visualizer.*
import esmeta.util.BaseUtils.*
import esmeta.util.BoolOption

/** `dump` phase */
case object Dump extends Phase[CFG, Unit] {
  val name = "dump"
  val help =
    "dumps the resources required by the debugger and visualizer. (for internal use)"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    if (!config.debugger && !config.visualizer) {
      raise(
        "At least one of `debugger` or `visualizer` must be enabled.",
      )
    }
    if (config.debugger) DumpDebugger(cfg)
    if (config.visualizer) DumpVisualizer(cfg)
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debugger",
      BoolOption(_.debugger = _),
      "dump resources for the standalone debugger",
    ),
    (
      "visualizer",
      BoolOption(_.visualizer = _),
      "dump resources for the visualizer",
    ),
  )
  case class Config(
    var debugger: Boolean = false,
    var visualizer: Boolean = false,
  )
}
