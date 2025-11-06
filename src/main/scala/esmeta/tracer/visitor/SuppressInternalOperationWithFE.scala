package esmeta.tracer
package visitor

import TraceDiff.*

final class SuppressInternalOperationWithFE(using tk: TargetKind)
  extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    def isContainableCandidate(node: TraceTree.Node): Boolean =
      node.open match
        case ifp: Trace.InvokeFunPre if ifp.isInternal == Some(true) => true
        case gfp: Trace.GetFieldPre if gfp.isInternal == Some(true)  => true
        case _                                                       => false

    def isInternalOperation(node: TraceTree.Node): Boolean =
      node.open match
        case gf: Trace.GetField if gf.isInternal == Some(true)       => true
        case gfp: Trace.GetFieldPre if gfp.isInternal == Some(true)  => true
        case inf: Trace.InvokeFun if inf.isInternal == Some(true)    => true
        case ifp: Trace.InvokeFunPre if ifp.isInternal == Some(true) => true
        case _                                                       => false

    def isFunctionEnter(node: TraceTree.Node): Boolean =
      node.open match
        case _: Trace.FunctionEnter => true
        case _                      => false

    def countFunctionEnters(node: TraceTree.Node): Int = {
      def visit(n: TraceTree.Node): Int =
        if isFunctionEnter(n) then 1
        else n.children.map(visit).sum
      visit(node)
    }

    def unwrapInternal(
      node: TraceTree.Node,
    ): (Trace, Vector[TraceTree.Node], Option[Trace]) =
      def stripNested(
        nodes: Vector[TraceTree.Node],
      ): Vector[TraceTree.Node] =
        nodes.flatMap { child =>
          if (isContainableCandidate(child)) stripNested(child.children)
          else Vector(child.copy(children = stripNested(child.children)))
        }
      (node.open, stripNested(node.children), node.close)

    def markInternalIgnored(n: DiffNode): DiffNode =
      n.copy(tag = DiffTag.Ignored("internal InvokeFun or GetField", Side.Left))

    def mergeWithRightFunctionEnters(
      leftNode: TraceTree.Node,
      rightNodes: Vector[TraceTree.Node],
    ): Vector[DiffNode] = {
      val (lopen, leftDesc, lclose) = unwrapInternal(leftNode)
      val leftWrapped = TraceTree.Node(
        lopen,
        close = lclose,
        children = leftDesc,
      )
      val rightWrapped = TraceTree.Node(
        Trace.Unpacked(),
        close = Some(Trace.Unpacked()),
        children = rightNodes,
      )
      val merged = TraceDiff.diff(
        Vector(leftWrapped),
        Vector(rightWrapped),
      )
      val mergedIgnore = Ignore.apply(TraceDiff.DiffTree(merged))
      mergedIgnore.roots
    }

    def collectAhead(
      currentLeft: TraceTree.Node,
    ): Option[
      (
        Vector[(TraceTree.Node, Int)],
        Vector[TraceTree.Node],
        Vector[DiffNode],
        Int,
      ),
    ] = {
      var offset = 1
      var consume = 1
      val leftNodes = Vector.newBuilder[(TraceTree.Node, Int)]
      val rightFEs = Vector.newBuilder[TraceTree.Node]
      val skipped = Vector.newBuilder[DiffNode]
      var leftCount = 0
      var rightCount = 0
      val currentCount = countFunctionEnters(currentLeft)
      if (currentCount == 0) return None
      leftNodes += ((currentLeft, currentCount))
      leftCount += currentCount
      var done = false
      while (!done) {
        cursor.peek(offset) match
          case Some(next) =>
            next.tag match
              case DiffTag.Ignored(_, _) =>
                skipped += next
                offset += 1
                consume = offset
              case DiffTag.LeftOnly =>
                next.left match
                  case Some(l) if isInternalOperation(l) =>
                    val feCount = countFunctionEnters(l)
                    if (isContainableCandidate(l) && feCount > 0) {
                      leftNodes += ((l, feCount))
                      leftCount += feCount
                    } else skipped += markInternalIgnored(next)
                    offset += 1
                    consume = offset
                  case _ => done = true
              case DiffTag.RightOnly =>
                next.right match
                  case Some(r) if isFunctionEnter(r) =>
                    rightFEs += r
                    rightCount += 1
                    offset += 1
                    consume = offset
                    if (rightCount >= leftCount) done = true
                  case _ => done = true
              case _ => done = true
          case None => done = true
      }
      val leftSeq = leftNodes.result()
      val rightSeq = rightFEs.result()
      if (rightSeq.nonEmpty && rightSeq.size >= leftCount)
        Some((
          leftSeq,
          rightSeq.take(leftCount),
          skipped.result(),
          consume,
        ))
      else None
    }

    val DiffNode(tag, left, _, _, _) = node;
    (tag -> left) match
      case (DiffTag.LeftOnly -> Some(left)) if isContainableCandidate(left) =>
        if (countFunctionEnters(left) > 0) {
          collectAhead(left) match
            case Some((leftSeq, rightSeq, skippedNodes, consume)) =>
              var idx = 0
              val mergedPairs = leftSeq.flatMap { (ln, c) =>
                val slice = rightSeq.slice(idx, idx + c)
                idx += c
                mergeWithRightFunctionEnters(ln, slice)
              }
              Decision.Replace(mergedPairs ++ skippedNodes, consume = consume)
            case None => Decision.Keep(node)
        } else
          Decision.Replace(
            Vector(
              node.copy(tag =
                DiffTag.Ignored("internal InvokeFun or GetField", Side.Left),
              ),
            ),
          )
      case _ => Decision.Keep(node)
  }
}
