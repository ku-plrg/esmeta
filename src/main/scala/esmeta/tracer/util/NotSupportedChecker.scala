package esmeta.tracer.util

import esmeta.cfg.*
import esmeta.error.NotSupported
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.state.*
import esmeta.tracer.*
import esmeta.util.BaseUtils.*

class NotSupportedChecker(
  st: State,
  timeLimit: Option[Int] = None,
) extends Interpreter(
    st = st,
    tyCheck = false,
    log = false,
    detail = false,
    logPW = None,
    timeLimit = timeLimit,
  ) {

  val ns = Set("PerformEval")

  lazy val cfg = st.cfg

  lazy val defineProperty =
    cfg.funcs
      .find(f => f.name == "INTRINSICS.Object.defineProperty")
      .getOrElse(raise("Cannot find target function."))

  lazy val targetExpr = "(! (? O: Record[Object]))"

  override def eval(cursor: Cursor): Boolean = {
    if (cursor.isInstanceOf[ExitCursor] && ns.contains(cursor.func.name)) {
      throw NotSupported(
        s"Tracing tests with ${cursor.func.name} is not supported.",
      )
    }
    // 18414: if (! (? O: Record[Object])) then 18415 else 18418
    if ({
      lazy val isInstanceOf = cursor.isInstanceOf[NodeCursor]
      lazy val nodeCursor = cursor.asInstanceOf[NodeCursor]
      lazy val isBranch = nodeCursor.node match
        case _: Block | _: Call => false
        case branch: Branch     => true
      lazy val branch = nodeCursor.node.asInstanceOf[Branch]

      isInstanceOf &&
      nodeCursor.func == defineProperty &&
      isBranch &&
      branch.cond.toString == targetExpr
    }) {
      val o = st.apply(Name("O"))
      val isIntrinsic = o.isAddr && (o.asAddr match
        case NamedAddr(name) => name.startsWith("INTRINSICS.")
        case _               => false
      )
      if (isIntrinsic) {
        throw NotSupported(
          s"Define property for NamedAddr(Intrinsic) is not supported.",
        )
      }
    }
    super.eval(cursor)
  }

}
