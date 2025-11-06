package esmeta.tracer
package visitor

import TraceDiff.*

final class RightSideNotCallable extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> right) match
      case (DiffTag.RightOnly -> Some(right)) =>
        right.open match
          case iv: Trace.InvokeFunPre
              // we can not check by this - see `not-constructable.js` test
              // if iv.f == "<not callable>"
              =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored("right-side not callable", Side.Right),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
