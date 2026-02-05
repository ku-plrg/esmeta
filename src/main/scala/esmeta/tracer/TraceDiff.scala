package esmeta.tracer

import scala.collection.mutable.ArrayBuffer

object TraceDiff {
  import TraceTree.Node

  enum Side {
    case Left
    case Right
  }

  enum DiffTag {
    case Same
    case Ignored(reason: String, side: Side)
    case Different
    case LeftOnly
    case RightOnly
  }

  case class DiffTree(roots: Vector[DiffNode]) extends TracerElem {

    def isAllSame: Boolean = {
      def isSameOrIgnored(tag: DiffTag): Boolean = tag match
        case DiffTag.Same          => true
        case DiffTag.Ignored(_, _) => true
        case _                     => false
      def allSame(node: DiffNode): Boolean =
        isSameOrIgnored(node.tag) && node.children.forall(allSame)
      roots.forall(allSame)
    }

    def applyIgnoring: DiffTree = {
      // tree를 돌다가 필요한 시점에 다음 노드들을 "미리 보고", 해당 부분을 "통째로 교체" 하는 방식으로 계속해서 순회
      ???
    }
  }

  case class DiffNode(
    tag: DiffTag,
    left: Option[Node],
    right: Option[Node],
    closeSame: Boolean,
    children: Vector[DiffNode],
  )

  // compute diff tree from two trace trees, without ignoring
  def diff(
    left: Vector[TraceTree.Node],
    right: Vector[TraceTree.Node],
  ): Vector[DiffNode] = {
    // simply use alignPure
    val aligned = alignPure(left, right)
    val out = aligned.map {
      case (Some(l), Some(r)) =>
        val closeSame = closeMatches(l, r)
        val tag =
          if (
            matches(l, r)
            && closeSame
          )
            DiffTag.Same
          else DiffTag.Different
        val children = diff(l.children, r.children)
        DiffNode(tag, Some(l), Some(r), closeSame = closeSame, children)
      case (Some(l), None) =>
        DiffNode(
          DiffTag.LeftOnly,
          Some(l),
          None,
          closeSame = false,
          diff(l.children, Vector.empty),
        )
      case (None, Some(r)) =>
        DiffNode(
          DiffTag.RightOnly,
          None,
          Some(r),
          closeSame = false,
          diff(Vector.empty, r.children),
        )
      case (None, None) =>
        DiffNode(
          DiffTag.Same,
          None,
          None,
          closeSame = true,
          Vector.empty,
        )
    }
    out
  }

  // align, with no ignoring
  private def alignPure(
    left: Vector[Node],
    right: Vector[Node],
  ): Vector[(Option[Node], Option[Node])] = {
    val m = left.length
    val n = right.length
    val dp = Array.ofDim[Int](m + 1, n + 1)

    var i = m - 1
    while (i >= 0) {
      var j = n - 1
      while (j >= 0) {
        if (matches(left(i), right(j)))
          dp(i)(j) = dp(i + 1)(j + 1) + 1
        else
          dp(i)(j) = math.max(dp(i + 1)(j), dp(i)(j + 1))
        j -= 1
      }
      i -= 1
    }

    val aligned = ArrayBuffer.empty[(Option[Node], Option[Node])]
    i = 0
    var j = 0
    while (i < m && j < n) {
      if (matches(left(i), right(j))) {
        aligned.append((Some(left(i)), Some(right(j))))
        i += 1
        j += 1
      } else if (dp(i + 1)(j) >= dp(i)(j + 1)) {
        aligned.append((Some(left(i)), None))
        i += 1
      } else {
        aligned.append((None, Some(right(j))))
        j += 1
      }
    }
    while (i < m) {
      aligned.append((Some(left(i)), None))
      i += 1
    }
    while (j < n) {
      aligned.append((None, Some(right(j))))
      j += 1
    }

    aligned.toVector
  }

  @deprecated
  private def alignByOpen(
    left: Vector[Node],
    right: Vector[Node],
  ): Vector[(Option[Node], Option[Node])] = {
    val m = left.length
    val n = right.length
    val dp = Array.ofDim[Int](m + 1, n + 1)

    var i = m - 1
    while (i >= 0) {
      var j = n - 1
      while (j >= 0) {
        if (matches(left(i), right(j)))
          dp(i)(j) = dp(i + 1)(j + 1) + 1
        else
          dp(i)(j) = math.max(dp(i + 1)(j), dp(i)(j + 1))
        j -= 1
      }
      i -= 1
    }

    val aligned = ArrayBuffer.empty[(Option[Node], Option[Node])]
    i = 0
    var j = 0
    while (i < m && j < n) {
      if (matches(left(i), right(j))) {
        aligned.append((Some(left(i)), Some(right(j))))
        i += 1
        j += 1
      } else if (dp(i + 1)(j) >= dp(i)(j + 1)) {
        if (isIgnorable(left(i), Side.Left)) {
          aligned.append((Some(left(i)), None))
          i += 1
        } else if (isIgnorable(right(j), Side.Right)) {
          aligned.append((None, Some(right(j))))
          j += 1
        } else {
          aligned.append((Some(left(i)), None))
          i += 1
        }
      } else {
        if (isIgnorable(right(j), Side.Right)) {
          aligned.append((None, Some(right(j))))
          j += 1
        } else if (isIgnorable(left(i), Side.Left)) {
          aligned.append((Some(left(i)), None))
          i += 1
        } else {
          aligned.append((None, Some(right(j))))
          j += 1
        }
      }
    }
    while (i < m) {
      aligned.append((Some(left(i)), None))
      i += 1
    }
    while (j < n) {
      aligned.append((None, Some(right(j))))
      j += 1
    }

    aligned.toVector
  }

  private def findFunctionEnterDesc(node: Node): Option[Node] =
    if (isFunctionEnter(node)) Some(node)
    else node.children.view.flatMap(findFunctionEnterDesc).headOption

  private def isInternalInvokeFunPre(node: Node): Boolean =
    node.open match
      case Trace.InvokeFunPre(_, Some(true)) => true
      case _                                 => false

  private def isFunctionEnter(node: Node): Boolean =
    node.open match
      case _: Trace.FunctionEnter => true
      case _                      => false

  private def matches(left: Node, right: Node): Boolean =
    traceMatches(left.open, right.open)

  private def closeMatches(left: Node, right: Node): Boolean =
    (left.close, right.close) match
      case (Some(l), Some(r)) => traceMatches(l, r)
      case (None, None)       => true
      case _                  => false

  private def traceMatches(left: Trace, right: Trace): Boolean =
    (left, right) match
      case (l: Trace.GetField, r: Trace.GetField) =>
        l.prop == "<side effect>" || l.prop == r.prop
      case (l: Trace.GetFieldPre, r: Trace.GetFieldPre) =>
        r.prop == "<side effect>" || l.prop == r.prop
      case (a: Trace.PutField, b: Trace.PutField) =>
        b.prop == "<side effect>" || a.prop == b.prop
      case (a: Trace.PutFieldPre, b: Trace.PutFieldPre) =>
        b.prop == "<side effect>" || a.prop == b.prop
      case (l: Trace.InvokeFun, r: Trace.InvokeFun) =>
        // jalangi incorrectly handles NamedEvaluation
        r.f == "" || r.f == "bound " || l.f == r.f
      case (l: Trace.InvokeFunPre, r: Trace.InvokeFunPre) =>
        // jalangi incorrectly handles NamedEvaluation
        r.f == "" || r.f == "bound " || l.f == r.f
      case (l: Trace.Binary, r: Trace.Binary) =>
        l.op == r.op
      case (u: Trace.Unary, v: Trace.Unary) =>
        u.op == v.op
      case (_: Trace.FunctionEnter, Trace.FunctionEnter("")) =>
        true // jalangi incorrectly handles NamedEvaluation
      case (_: Trace.FunctionExit, Trace.FunctionExit("todo")) =>
        true // jalangi not gives name in FunctionExit hook
      case (_: Trace.ForInObject, _: Trace.ForInObject) =>
        true // TODO object equality

      // deprecate this cases - this will be handled by ignore rules
      // case (Trace.Read("undefined"), Trace.Literal("undefined")) => true
      // case (Trace.Literal("undefined"), Trace.Read("undefined")) => true

      case (_, _: Trace.Unpacked) => true
      case (_: Trace.Unpacked, _) => true
      case _                      => left.matches(right)

  @deprecated
  private def isIgnorable(node: Node, side: Side): Boolean =
    ignoredReason(node.open, side).isDefined

  @deprecated
  private def ignoredReason(trace: Trace, side: Side): Option[String] =
    trace match
      case gf: Trace.GetField if gf.isInternal == Some(true) =>
        Some(s"internal field access")
      case gfp: Trace.GetFieldPre if gfp.isInternal == Some(true) =>
        Some(s"internal field access")
      case gf: Trace.GetField if gf.isGlobal == Some(true) =>
        Some(s"GetField(${gf.prop}) global")
      case gfp: Trace.GetFieldPre if gfp.isGlobal == Some(true) =>
        Some(s"GetFieldPre(${gfp.prop}) global")
      case Trace.Literal("function") if side == Side.Right =>
        Some(
          "function literals are not hooked properly in jalangi(right) trace, so ignore in left trace",
        )
      case Trace.UnaryPre("void") if side == Side.Left =>
        Some("void is not hooked in Jalangi")
      case u: Trace.Unary if u.op == "void" && side == Side.Left =>
        Some("void is not hooked in Jalangi")
      case i: Trace.InvokeFun if i.isInternal.contains(true) =>
        Some("internal InvokeFun")
      case i: Trace.InvokeFunPre if i.isInternal.contains(true) =>
        Some("internal InvokeFunPre")
      case Trace.Read("eval") if side == Side.Left =>
        Some("eval is not hooked in right trace, so ignore in left trace")
      case _ => None

}
