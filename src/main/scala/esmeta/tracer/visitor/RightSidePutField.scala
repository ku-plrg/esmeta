package esmeta.tracer
package visitor

import TraceDiff.*

/**
  * TODO putfiled 시점 문제로 인해 실패한 경우도 기록에 남게 됨 - 근데 이건 오라클 문제에 가까워서 명세 후킹을  수정해야할듯.
  */
final class RightSidePutField extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> right) match
      case (DiffTag.RightOnly -> Some(right)) =>
        right.open match
          case pf: Trace.PutFieldPre =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored("right-side PutField", Side.Right),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
