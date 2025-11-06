package esmeta.tracer
package visitor

import TraceDiff.*

final class RightSideDeclareThenFunctionLiteral extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> left) match
      case (DiffTag.LeftOnly -> Some(l)) =>
        l.open match
          case Trace.Declare(_) =>
            cursor.peek(1).flatMap(_.left) match
              case Some(next) =>
                next.open match
                  case Trace.Literal("function") =>
                    Decision.Replace(
                      Vector(
                        node.copy(tag =
                          DiffTag.Ignored(
                            "right-side declare then function literal",
                            Side.Right,
                          ),
                        ),
                      ),
                    )
                  case _ => Decision.Keep(node)
              case None => Decision.Keep(node)
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
