package esmeta.tracer
package visitor

import TraceDiff.*

// 일단 그냥 무시해보자
final class RightSideDeclare extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> right) match
      case (DiffTag.RightOnly -> Some(right)) =>
        right.open match
          case de: Trace.Declare =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored("right-side declare", Side.Right),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
