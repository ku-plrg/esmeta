package esmeta.tracer

import scala.collection.mutable.ArrayBuffer

case class TraceTree private (
  roots: Vector[TraceTree.Node],
) extends TracerElem {

  def diff(that: TraceTree): TraceDiff.DiffTree =
    TraceDiff.DiffTree(TraceDiff.diff(this.roots, that.roots))
}

object TraceTree {
  case class Node(
    open: Trace,
    close: Option[Trace] = None,
    children: Vector[Node] = Vector.empty,
  )

  private class NodeBuilder(val open: Trace) {
    var close: Option[Trace] = None
    val children: ArrayBuffer[NodeBuilder] = ArrayBuffer.empty

    def toNode: Node =
      Node(open, close, children.toVector.map(_.toNode))
  }

  def apply(traces: Seq[Trace]): TraceTree = {
    val roots = ArrayBuffer.empty[NodeBuilder]
    val stack = ArrayBuffer.empty[NodeBuilder]

    def attach(node: NodeBuilder): Unit =
      if (stack.nonEmpty) stack.last.children.append(node)
      else roots.append(node)

    traces.foreach { trace =>
      if (trace.isPre == Some(true)) {
        val node = new NodeBuilder(trace)
        attach(node)
        stack.append(node)
      } else if (trace.isPre == Some(false)) {
        if (stack.nonEmpty && (stack.last.open pairs trace)) {
          stack.last.close = Some(trace)
          stack.remove(stack.size - 1)
        } else {
          attach(new NodeBuilder(trace))
        }
      } else {
        attach(new NodeBuilder(trace))
      }
    }

    TraceTree(roots.toVector.map(_.toNode))
  }

}
