package esmeta.tracer
package visitor

import TraceDiff.*
import esmeta.util.BaseUtils.raise

/** |- [LeftOnly] L: UnaryPre(prefix++)
  * | `- [LeftOnly] L: Write(x)
  * |- L: Unary(prefix++)
  * |- [RightOnly] R: UnaryPre(+)
  * |- R: Unary(+)
  * |- [RightOnly] R: Literal(number)
  * |- [RightOnly] R: BinaryPre(+)
  * |- R: Binary(+) `- [RightOnly] R: Write(x)
  */

/** | | |- [LeftOnly] L: UnaryPre(prefix++)
  * | | | `- [LeftOnly] L: Write(i)
  * | | |- L: Unary(prefix++)
  * | | |- [RightOnly] R: UnaryPre(+)
  * | | |- R: Unary(+)
  * | | |- [RightOnly] R: Literal(number)
  * | | |- [Same] BinaryPre(+) <- !!!!!! peeking하면서 RightOnly 4개를 세어서 ignore하기
  * | | |- Binary(+)
  * | | |- [RightOnly] R: Write(i)
  * | | `- [RightOnly] R: BinaryPre(+)
  * | | `- R: Binary(+)
  */

/** @notes
  *   this should be used as last order as possible
  */
final class IncDecOperators extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val reason = "prefix++ expansion"

    def ignoreSubtree(node: DiffNode, side: Side): DiffNode =
      node.copy(
        tag = DiffTag.Ignored(reason, side),
        children = node.children.map(ignoreSubtree(_, side)),
      )

    def leftOnlyPrefixTarget(node: DiffNode): Option[String] = node match
      case DiffNode(DiffTag.LeftOnly, Some(left), None, _, children) =>
        left.open match
          case Trace.UnaryPre("prefix++" | "prefix--") =>
            Some("<unused>")
          // children match
          //   case Vector(
          //         DiffNode(DiffTag.LeftOnly, Some(childLeft), None, _, _),
          //       ) =>
          //     childLeft.open match
          //       case Trace.Write(name) => Some(name)
          //       case _                 => None
          //   case _ => None
          case _ => None
      case _ => None

    def leftOnlyPostfixTarget(node: DiffNode): Option[String] = node match
      case DiffNode(DiffTag.LeftOnly, Some(left), None, _, children) =>
        left.open match
          case Trace.UnaryPre("postfix++" | "postfix--") =>
            Some("<unused>")
          // children match
          //   case Vector(
          //         DiffNode(DiffTag.LeftOnly, Some(childLeft), None, _, _),
          //       ) =>
          //     childLeft.open match
          //       case Trace.Write(name) => Some(name)
          //       case _                 => None
          //   case _ => None
          case _ => None
      case _ => None

    def countRightonly(target: Int): Option[Int] = {
      var i = 0
      var error = false
      var countRightonly = 0
      while (!error && countRightonly < target) {
        cursor.peek(i) match
          case Some(n) =>
            n.tag match
              case DiffTag.RightOnly => countRightonly += 1
              case _                 =>
          case None => error = true
        i += 1
      }

      if (error) None
      else if (countRightonly == target) Some(i)
      else None
    }

    (leftOnlyPrefixTarget(node), leftOnlyPostfixTarget(node)) match
      case (Some(name), None) =>
        val consume = countRightonly(4).getOrElse(
          raise(
            s"Expected to find 4 RightOnly nodes after prefix++/-- Write($name)",
          ),
        )

        Decision.Replace(
          Vector(
            ignoreSubtree(node, Side.Left),
          ).appendedAll(
            (0 until (consume - 1)).map(j =>
              ignoreSubtree(cursor.peek(j).get, Side.Right),
            ),
          ),
          consume = consume,
          recurse = false,
        )
      case (None, Some(name)) =>
        val consume = countRightonly(6).getOrElse(
          raise(
            s"Expected to find 6 RightOnly nodes after postfix++ Write($name)",
          ),
        )

        Decision.Replace(
          Vector(
            ignoreSubtree(node, Side.Left),
          ).appendedAll(
            (0 until (consume - 1)).map(j =>
              ignoreSubtree(cursor.peek(j).get, Side.Right),
            ),
          ),
          consume = consume,
          recurse = false,
        )
      case (Some(_), Some(_)) =>
        raise("Cannot have both prefix++ and postfix++ targets")
      case (None, None) => Decision.Keep(node)
  }
}
