package esmeta.tracer
package visitor

import TraceDiff.*

/** |- [LeftOnly] L: UnaryPre(postfix--)
  * | `- [LeftOnly] L: Write(i)
  * |- L: Unary(postfix--)
  * |- [RightOnly] R: UnaryPre(+)
  * |- R: Unary(+)
  * |- [RightOnly] R: Literal(number)
  * |- [RightOnly] R: BinaryPre(-)
  * |- R: Binary(-)
  * |- [RightOnly] R: Write(i)
  * |- [RightOnly] R: Literal(number)
  * |- [RightOnly] R: BinaryPre(+)
  * |- R: Binary(+)
  */
@deprecated
final class PostfixMinus extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val reason = "postfix-- expansion"

    def ignoreSubtree(node: DiffNode, side: Side): DiffNode =
      node.copy(
        tag = DiffTag.Ignored(reason, side),
        children = node.children.map(ignoreSubtree(_, side)),
      )

    def leftOnlyPostfixTarget(node: DiffNode): Option[String] = node match
      case DiffNode(DiffTag.LeftOnly, Some(left), None, _, children) =>
        left.open match
          case Trace.UnaryPre("postfix--") =>
            children match
              case Vector(
                    DiffNode(DiffTag.LeftOnly, Some(childLeft), None, _, _),
                  ) =>
                childLeft.open match
                  case Trace.Write(name) => Some(name)
                  case _                 => None
              case _ => None
          case _ => None
      case _ => None

    def rightOnlyUnaryPre(op: String)(node: DiffNode): Boolean = node match
      case DiffNode(DiffTag.RightOnly, None, Some(right), _, _) =>
        right.open match
          case Trace.UnaryPre(`op`) =>
            right.close match
              case Some(Trace.Unary(`op`)) => true
              case _                       => false
          case _ => false
      case _ => false

    def rightOnlyBinaryPre(op: String)(node: DiffNode): Boolean = node match
      case DiffNode(DiffTag.RightOnly, None, Some(right), _, _) =>
        right.open match
          case Trace.BinaryPre(`op`) =>
            right.close match
              case Some(Trace.Binary(`op`)) => true
              case _                        => false
          case _ => false
      case _ => false

    def rightOnlyLiteralNumber(node: DiffNode): Boolean = node match
      case DiffNode(DiffTag.RightOnly, None, Some(right), _, _) =>
        right.open match
          case Trace.Literal("number") => true
          case _                       => false
      case _ => false

    def rightOnlyWrite(name: String)(node: DiffNode): Boolean = node match
      case DiffNode(DiffTag.RightOnly, None, Some(right), _, _) =>
        right.open match
          case Trace.Write(`name`) => true
          case _                   => false
      case _ => false

    leftOnlyPostfixTarget(node) match
      case Some(name) =>
        val n1 = cursor.peek(1)
        val n2 = cursor.peek(2)
        val n3 = cursor.peek(3)
        val n4 = cursor.peek(4)
        val n5 = cursor.peek(5)
        val n6 = cursor.peek(6)

        (n1, n2, n3, n4, n5, n6) match
          case (
                Some(a),
                Some(b),
                Some(c),
                Some(d),
                Some(e),
                Some(f),
              )
              if rightOnlyUnaryPre("+")(a)
              && rightOnlyLiteralNumber(b)
              && rightOnlyBinaryPre("-")(c)
              && rightOnlyWrite(name)(d)
              && rightOnlyLiteralNumber(e)
              && rightOnlyBinaryPre("+")(f) =>
            Decision.Replace(
              Vector(
                ignoreSubtree(node, Side.Left),
                ignoreSubtree(a, Side.Right),
                ignoreSubtree(b, Side.Right),
                ignoreSubtree(c, Side.Right),
                ignoreSubtree(d, Side.Right),
                ignoreSubtree(e, Side.Right),
                ignoreSubtree(f, Side.Right),
              ),
              consume = 7,
              recurse = false,
            )
          case _ => Decision.Keep(node)
      case None => Decision.Keep(node)
  }
}
