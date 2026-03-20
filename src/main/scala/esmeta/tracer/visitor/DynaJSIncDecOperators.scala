package esmeta.tracer
package visitor

import TraceDiff.*

/** ESMeta emits prefix/postfix increment/decrement as: [LeftOnly] L:
  * UnaryPre(prefix++) `- [LeftOnly] L: Write(x) while DynaJS emits it as:
  * [RightOnly] R: UnaryPre(++) `- [RightOnly] R: BinaryPre(+) `- [RightOnly] R:
  * Write(x) This filter marks both sides as Ignored so they are treated as
  * equivalent.
  *
  * @notes
  *   this should be used as late as possible in the pipeline
  */
final class DynaJSIncDecOperators extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val reason = "prefix/postfix inc/dec expansion (dynajs)"

    def ignoreSubtree(n: DiffNode, side: Side): DiffNode =
      n.copy(
        tag = DiffTag.Ignored(reason, side),
        children = n.children.map(ignoreSubtree(_, side)),
      )

    def isLeftOnlyIncDec(n: DiffNode): Boolean = n match
      case DiffNode(DiffTag.LeftOnly, Some(left), None, _, _) =>
        left.open match
          case Trace.UnaryPre(op) =>
            op == "prefix++" || op == "prefix--" ||
            op == "postfix++" || op == "postfix--"
          case _ => false
      case _ => false

    def isRightOnlyIncDec(n: DiffNode): Boolean = n match
      case DiffNode(DiffTag.RightOnly, None, Some(right), _, _) =>
        right.open match
          case Trace.UnaryPre(op) => op == "++" || op == "--"
          case _                  => false
      case _ => false

    if (!isLeftOnlyIncDec(node)) return Decision.Keep(node)

    // Look ahead for the matching RightOnly UnaryPre, skipping Ignored nodes
    var offset = 1
    var foundOffset = -1
    var searching = true
    while (searching) {
      cursor.peek(offset) match
        case Some(next) =>
          next.tag match
            case DiffTag.Ignored(_, _) => offset += 1
            case _ =>
              if (isRightOnlyIncDec(next)) foundOffset = offset
              searching = false
        case None => searching = false
    }

    if (foundOffset < 0) return Decision.Keep(node)

    val consume = foundOffset + 1
    val replacement = Vector.newBuilder[DiffNode]
    replacement += ignoreSubtree(node, Side.Left)
    for (j <- 1 until foundOffset) {
      replacement += cursor
        .peek(j)
        .get // pass through intermediate Ignored nodes
    }
    replacement += ignoreSubtree(cursor.peek(foundOffset).get, Side.Right)

    Decision.Replace(replacement.result(), consume = consume, recurse = false)
  }
}
