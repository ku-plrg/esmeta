package esmeta.tracer
package visitor

import TraceDiff.*

final class SuppressInternalOperation extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    def hasFunctionEnterDescendant(node: TraceTree.Node): Boolean =
      node.children.exists { child =>
        child.open match
          case _: Trace.FunctionEnter => true
          case _                      => hasFunctionEnterDescendant(child)
      }

    val DiffNode(tag, left, right, closeSame, children) = node;
    (tag -> left) match
      case (DiffTag.LeftOnly -> Some(left)) =>
        left.open match
          case gf: Trace.GetFieldPre if gf.isInternal == Some(true) =>
            if (hasFunctionEnterDescendant(left)) Decision.Keep(node)
            else
              Decision.Replace(
                Vector(
                  node.copy(tag =
                    DiffTag.Ignored("internal GetField", Side.Left),
                  ),
                ),
              )
          case ifp: Trace.InvokeFunPre if ifp.isInternal == Some(true) =>
            if (hasFunctionEnterDescendant(left)) Decision.Keep(node)
            else
              Decision.Replace(
                Vector(
                  node.copy(tag =
                    DiffTag.Ignored("internal InvokeFun", Side.Left),
                  ),
                ),
              )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
