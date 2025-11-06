package esmeta.tracer
package visitor

import TraceDiff.*

// sequential cursor with lookahead, scoped to a sibling list
final case class VisitCtx(parents: List[DiffNode], siblingIndex: Int)

final class SeqCursor(nodes: Vector[DiffNode], parents: List[DiffNode]) {
  private var idx: Int = 0

  def index: Int = idx
  def hasNext: Boolean = idx < nodes.length
  def peek(offset: Int = 0): Option[DiffNode] = {
    val i = idx + offset
    if (i >= 0 && i < nodes.length) Some(nodes(i)) else None
  }
  def peekCtx(offset: Int = 0): Option[VisitCtx] = {
    val i = idx + offset
    if (i >= 0 && i < nodes.length) Some(VisitCtx(parents, i)) else None
  }
  def ctx: VisitCtx = VisitCtx(parents, idx)
  def advance(n: Int): Unit =
    idx = math.min(nodes.length, idx + math.max(n, 0))
}

sealed trait Decision {
  def consume: Int
  def recurse: Boolean
}
object Decision {
  final case class Keep(node: DiffNode, recurse: Boolean = true)
    extends Decision {
    val consume: Int = 1
  }
  final case class Replace(
    nodes: Vector[DiffNode],
    consume: Int = 1,
    recurse: Boolean = true,
  ) extends Decision
  final case class Drop(consume: Int) extends Decision {
    val recurse: Boolean = false
  }
}

trait TreeVisitor {
  def traverse = DiffTreeTraverser(this).run
  def onNode(node: DiffNode, cursor: SeqCursor, ctx: VisitCtx): Decision
}

final class DiffTreeTraverser(visitor: TreeVisitor) {

  object ops {
    def children(node: DiffNode): Vector[DiffNode] = node.children
    def withChildren(node: DiffNode, children: Vector[DiffNode]): DiffNode =
      node.copy(children = children)
  }

  def run(roots: Vector[DiffNode]): Vector[DiffNode] = visitSeq(roots, Nil)

  private def visitSeq(
    nodes: Vector[DiffNode],
    parents: List[DiffNode],
  ): Vector[DiffNode] = {
    val cursor = new SeqCursor(nodes, parents)
    val out = Vector.newBuilder[DiffNode]
    while (cursor.hasNext) {
      val node = cursor.peek(0).get
      visitor.onNode(node, cursor, cursor.ctx) match {
        case Decision.Keep(n, recurse) =>
          val updated =
            if (recurse) {
              val newChildren = visitSeq(ops.children(n), n :: parents)
              ops.withChildren(n, newChildren)
            } else n
          out += updated
          cursor.advance(1)
        case Decision.Replace(ns, consume, recurse) =>
          ns.foreach { n =>
            val updated =
              if (recurse) {
                val newChildren = visitSeq(ops.children(n), n :: parents)
                ops.withChildren(n, newChildren)
              } else n
            out += updated
          }
          cursor.advance(consume)
        case Decision.Drop(consume) =>
          cursor.advance(consume)
      }
    }
    out.result()
  }
}
