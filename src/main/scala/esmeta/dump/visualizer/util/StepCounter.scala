package esmeta.dump.visualizer.util

import esmeta.cfg.{CFG, Node}
import esmeta.interpreter.Interpreter
import esmeta.state.State
import esmeta.util.BaseUtils.*
import scala.util.{Try, Success, Failure}

private case class FoundStep(step: Int) extends Throwable

private class StepCounter(
  st: State,
  targetNodeId: Int,
  targetCallPath: String,
) extends Interpreter(st) {
  override def eval(node: Node): Unit =
    val currentCP = new VisualizerJsonProtocol(st.cfg)
      .parseCallPath(Some(st.context.callPath.toString))
      .getOrElse("")

    if (node.id == targetNodeId && (currentCP == targetCallPath)) {
      raise(stepCnt.toString)
    }

    super.eval(node)
}

object StepCounter:
  def count(code: String, targetNodeId: Int, targetCallPath: String)(
    printMsg: => Unit,
  )(using cfg: CFG): Int =
    Try {
      new StepCounter(cfg.init.from(code), targetNodeId, targetCallPath).result
    } match
      case Failure(exception: FoundStep) => exception.step
      case Success(value)                => 1
      case Failure(e)                    => throw e
