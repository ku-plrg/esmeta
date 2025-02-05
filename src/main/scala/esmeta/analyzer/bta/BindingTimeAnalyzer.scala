package esmeta.analyzer.bta

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => _, *, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** specification type analyzer for ECMA-262 */
class BindingTimeAnalyzer(
  val cfg: CFG,
) extends Analyzer
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with ViewDecl {

  /** worklist of control points */
  val worklist: Worklist[ControlPoint] = ???

  /** abstract transfer function */
  type AbsTransfer <: AbsTransferLike
  val transfer: AbsTransfer = ???

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean = ???

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean = ???

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String = ???

  val log: Boolean = ???

  /** logging the current analysis result */
  def logging: Unit = ???
}
