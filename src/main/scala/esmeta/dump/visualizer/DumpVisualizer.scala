package esmeta.dump.visualizer

import esmeta.cfg.*

object DumpVisualizer:
  def apply(cfg: CFG): Unit =
    DumpSecIdToFuncInfo(cfg)
    DumpStepToNodeId(cfg)
    DumpNodeIdToProgId(cfg)
    DumpNodeIdToTest262(cfg)
