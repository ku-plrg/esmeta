package esmeta.tracer
package visitor

import TraceDiff.*

final class LiteralAsRead extends TreeVisitor {
  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;
    def readToLiteral(readName: String): Option[String] = readName match
      case "undefined" => Some("undefined")
      case "null"      => Some("null")
      case "Infinity"  => Some("number")
      case "NaN"       => Some("number")
      case _           => None

    def replaceWithMatchingRightOnlyLiteral(
      readName: String,
      readNode: DiffNode,
    ): Option[Decision.Replace] = {
      val expected = readToLiteral(readName)
      if (expected.isEmpty) None
      else {
        val expectedValue = expected.get
        val nodes = scala.collection.mutable.ArrayBuffer.empty[DiffNode]
        val leftReadIdxs = scala.collection.mutable.ArrayBuffer.empty[Int]
        val rightLiteralIdxs = scala.collection.mutable.ArrayBuffer.empty[Int]
        nodes += readNode
        leftReadIdxs += 0
        var offset = 1
        var consume = 1
        var done = false
        while (!done) {
          cursor.peek(offset) match
            case Some(next) =>
              next.tag match
                case DiffTag.Ignored(_, _) =>
                  nodes += next
                  offset += 1
                  consume = offset
                case DiffTag.LeftOnly =>
                  next.left match
                    case Some(l) =>
                      l.open match
                        case Trace.Read(name)
                            if readToLiteral(name).contains(expectedValue) =>
                          nodes += next
                          leftReadIdxs += (nodes.size - 1)
                          offset += 1
                          consume = offset
                        case _ => done = true
                    case _ => done = true
                case DiffTag.RightOnly =>
                  next.right match
                    case Some(r) =>
                      r.open match
                        case Trace.Literal(valueType)
                            if expected.contains(
                              valueType,
                            ) =>
                          nodes += next
                          rightLiteralIdxs += (nodes.size - 1)
                          offset += 1
                          consume = offset
                        case _ => done = true
                    case _ => done = true
                case _ => done = true
            case None => done = true
        }

        val leftIdxs = leftReadIdxs.toVector
        val rightIdxs = rightLiteralIdxs.toVector
        val pairCount = math.min(leftIdxs.size, rightIdxs.size)
        if (pairCount == 0) None
        else {
          val updated = nodes.toArray
          (0 until pairCount).foreach { i =>
            val leftIdx = leftIdxs(i)
            val leftName = updated(leftIdx).left match
              case Some(l) =>
                l.open match
                  case Trace.Read(name) => name
                  case _                => readName
              case None => readName
            updated(leftIdx) = updated(leftIdx).copy(tag =
              DiffTag.Ignored(s"left-side Read($leftName)", Side.Left),
            )
            val rightIdx = rightIdxs(i)
            val valueType = updated(rightIdx).right match
              case Some(r) =>
                r.open match
                  case Trace.Literal(vt) => vt
                  case _                 => expectedValue
              case None => expectedValue
            updated(rightIdx) = updated(rightIdx).copy(tag =
              DiffTag.Ignored(s"right-side Literal($valueType)", Side.Right),
            )
          }
          Some(Decision.Replace(updated.toVector, consume = consume))
        }
      }
    }

    def hasMatchingLeftOnlyReadBehind(valueType: String): Boolean = {
      def isMatchingRead(readName: String): Boolean =
        readToLiteral(readName).contains(valueType)

      var offset = -1
      var found = false
      var done = false
      while (!done) {
        cursor.peek(offset) match
          case Some(prev) =>
            prev.tag match
              case DiffTag.Ignored(_, _) =>
                offset -= 1
              case DiffTag.LeftOnly =>
                prev.left match
                  case Some(l) =>
                    l.open match
                      case Trace.Read(name) if isMatchingRead(name) =>
                        found = true
                        done = true
                      case _ => done = true
                  case _ => done = true
              case _ => done = true
          case None => done = true
      }
      found
    }

    (tag, left, right) match
      case (DiffTag.LeftOnly, Some(left), _) =>
        left.open match
          case r @ Trace.Read(name) if readToLiteral(name).isDefined =>
            replaceWithMatchingRightOnlyLiteral(name, node)
              .getOrElse(Decision.Keep(node))
          case _ => Decision.Keep(node)
      case (DiffTag.RightOnly, _, Some(right)) =>
        right.open match
          case lit @ Trace.Literal(valueType)
              if hasMatchingLeftOnlyReadBehind(valueType) =>
            Decision.Replace(
              Vector(
                node.copy(tag =
                  DiffTag.Ignored(
                    s"right-side Literal(${lit.valueType})",
                    Side.Right,
                  ),
                ),
              ),
            )
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
