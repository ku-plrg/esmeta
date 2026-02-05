package esmeta.tracer
package visitor

import TraceDiff.*

/* to check cases such as `invoke-typeerror` */

final class RightSideThrown extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> right) match
      case (DiffTag.RightOnly -> Some(right)) =>
        right.close match
          case Some(iv: Trace.InvokeFun)
              // we can not check by this - see `not-constructable.js` test
              // if iv.f == "<not callable>"
              if iv.isThrown.contains(true) && right.children.isEmpty =>
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
