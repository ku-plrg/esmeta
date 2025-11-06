package esmeta.tracer
package visitor

import TraceDiff.*

final class RightSideFunctionLiteral extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> right) match
      case (DiffTag.RightOnly -> Some(right)) =>
        right.open match
          case Trace.Literal("function") =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored("right-side function literal", Side.Right),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
