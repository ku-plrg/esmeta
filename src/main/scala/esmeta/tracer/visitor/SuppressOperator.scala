package esmeta.tracer
package visitor

import TraceDiff.*

final class SuppressOperator extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> left) match
      case (DiffTag.LeftOnly -> Some(left)) =>
        left.open match
          case u: Trace.UnaryPre if u.op == "void" =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored("void is not hooked in jalangi", Side.Left),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
