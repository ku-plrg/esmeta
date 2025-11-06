package esmeta.tracer.util

import esmeta.error.NotSupported
import esmeta.interpreter.Interpreter
import esmeta.state.*

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

  override def eval(cursor: Cursor): Boolean = {
    if (cursor.isInstanceOf[ExitCursor] && ns.contains(cursor.func.name)) {
      throw NotSupported(
        s"Tracing tests with ${cursor.func.name} is not supported.",
      )
    }
    super.eval(cursor)
  }

}
