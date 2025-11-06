package esmeta.tracer
package visitor

import TraceDiff.*

final class GetFromGlobal extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> left) match
      case (DiffTag.LeftOnly -> Some(left)) =>
        left.open match
          case gf: Trace.GetFieldPre if gf.isGlobal == Some(true) =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored("get field from global", Side.Left),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
