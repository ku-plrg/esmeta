package esmeta.tracer
package visitor

import TraceDiff.*
import scala.collection.mutable

/** |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(assert)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(isPrimitive)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(compareArray)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(Test262Error)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare($DONOTEVALUATE)
  * |- [RightOnly] R: Declare(__isArray)
  * |- [RightOnly] R: Declare(__defineProperty)
  * |- [RightOnly] R: Declare(__getOwnPropertyDescriptor)
  * |- [RightOnly] R: Declare(__getOwnPropertyNames)
  * |- [RightOnly] R: Declare(__join)
  * |- [RightOnly] R: Declare(__push)
  * |- [RightOnly] R: Declare(__hasOwnProperty)
  * |- [RightOnly] R: Declare(__propertyIsEnumerable)
  * |- [RightOnly] R: Declare(nonIndexNumericPropertyName)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyProperty)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(isConfigurable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(isEnumerable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(isSameValue)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(isWritable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyCallableProperty)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyEqualTo)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyWritable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyNotWritable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyEnumerable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyNotEnumerable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyConfigurable)
  * |- [Ignored(Right, right-side function literal)] R: Literal(function)
  * |- [Same] Declare(verifyNotConfigurable)
  * |- [LeftOnly] L: Declare(__isArray)
  * |- [LeftOnly] L: Declare(__defineProperty)
  * |- [LeftOnly] L: Declare(__getOwnPropertyDescriptor)
  * |- [LeftOnly] L: Declare(__getOwnPropertyNames)
  * |- [LeftOnly] L: Declare(__join)
  * |- [LeftOnly] L: Declare(__push)
  * |- [LeftOnly] L: Declare(__hasOwnProperty)
  * |- [LeftOnly] L: Declare(__propertyIsEnumerable)
  * |- [LeftOnly] L: Declare(nonIndexNumericPropertyName)
  * |- [Same] Declare(verifyPrimordialProperty)
  * |- [Same] Declare(verifyPrimordialCallableProperty)
  * |- [Same] Read(assert)
  */
final class FirstDeclare extends TreeVisitor {
  private val pendingLeftIgnore
    : mutable.Map[List[DiffNode], mutable.Set[String]] = mutable.Map.empty
  private val pendingRightIgnore
    : mutable.Map[List[DiffNode], mutable.Set[String]] = mutable.Map.empty

  def onNode(
    node: DiffNode,
    cursor: SeqCursor,
    ctx: VisitCtx,
  ): Decision = {
    val DiffNode(tag, left, right, closeSame, children) = node;

    def declareName(node: DiffNode): Option[String] = {
      val leftName = node.left.flatMap {
        _.open match
          case Trace.Declare(name) => Some(name)
          case _                   => None
      }
      leftName.orElse {
        node.right.flatMap {
          _.open match
            case Trace.Declare(name) => Some(name)
            case _                   => None
        }
      }
    }

    def isIgnoredFunctionLiteral(node: DiffNode): Boolean = node.tag match
      case DiffTag.Ignored(_, Side.Right) =>
        node.right match
          case Some(r) => r.open == Trace.Literal("function")
          case None    => false
      case _ => false

    def isIgnoredInternal(node: DiffNode): Boolean = node.tag match
      case DiffTag.Ignored(reason, _) => true
      case _                          => false

    def isBoundary(node: DiffNode): Boolean =
      declareName(node).isEmpty
      && !isIgnoredFunctionLiteral(node)
      && !isIgnoredInternal(node)

    def pendingSet(
      map: mutable.Map[List[DiffNode], mutable.Set[String]],
    ): mutable.Set[String] = map.getOrElseUpdate(ctx.parents, mutable.Set.empty)

    def hasOppositeAhead(name: String, side: Side): Boolean = {
      var offset = 1
      var done = false
      while (!done) {
        cursor.peek(offset) match
          case Some(next) if isBoundary(next) =>
            done = true
          case Some(next) =>
            val isMatch = next match
              case DiffNode(DiffTag.LeftOnly, Some(l), None, _, _)
                  if side == Side.Right && l.open == Trace.Declare(name) =>
                true
              case DiffNode(DiffTag.RightOnly, None, Some(r), _, _)
                  if side == Side.Left && r.open == Trace.Declare(name) =>
                true
              case _ => false
            if (isMatch) return true
            offset += 1
          case None =>
            done = true
      }
      false
    }

    (tag, left, right) match
      case (DiffTag.RightOnly, None, Some(r)) =>
        r.open match
          case Trace.Declare(name) =>
            val pendingRight = pendingSet(pendingRightIgnore)
            if (pendingRight.remove(name)) {
              Decision.Keep(
                node.copy(tag =
                  DiffTag.Ignored("first-side declare reorder", Side.Right),
                ),
              )
            } else if (hasOppositeAhead(name, Side.Right)) {
              pendingSet(pendingLeftIgnore).add(name)
              Decision.Keep(
                node.copy(tag =
                  DiffTag.Ignored("first-side declare reorder", Side.Right),
                ),
              )
            } else Decision.Keep(node)
          case _ => Decision.Keep(node)
      case (DiffTag.LeftOnly, Some(l), None) =>
        l.open match
          case Trace.Declare(name) =>
            val pendingLeft = pendingSet(pendingLeftIgnore)
            if (pendingLeft.remove(name)) {
              Decision.Keep(
                node.copy(tag =
                  DiffTag.Ignored("first-side declare reorder", Side.Left),
                ),
              )
            } else if (hasOppositeAhead(name, Side.Left)) {
              pendingSet(pendingRightIgnore).add(name)
              Decision.Keep(
                node.copy(tag =
                  DiffTag.Ignored("first-side declare reorder", Side.Left),
                ),
              )
            } else Decision.Keep(node)
          case _ => Decision.Keep(node)
      case _ => Decision.Keep(node)
  }
}
