package esmeta.analyzer.flowsummary

import esmeta.flowsummary.{Path => APath, *}
import esmeta.flowsummary.Semantics.*
import esmeta.ir.{Local, Name}
import esmeta.util.Appender.*

/** abstract values */
trait AbsValueDecl { self: FlowSummaryAnalyzer =>

  /** abstract closures with captured values */
  case class AbsClo(fname: String, captured: Map[Name, AbsValue])

  /** A value is described by where it may come from (`srcs`, access paths
    * rooted at the receiver or an argument), which objects allocated during the
    * call it may point to (`locs`), which abstract closures it may be (`clos`),
    * and an optional constant used to prune infeasible branches.
    */
  case class AbsValue(
    srcs: Set[Src] = Set(),
    locs: Set[Loc] = Set(),
    clos: Set[AbsClo] = Set(),
    cst: Cst = Cst.Bot,
    rest: Set[Int] = Set(), // the remaining `ArgumentsList` from this offset
    maybeAbsent: Boolean = false,
    // elements popped from the front of the lists in `locs`
    // (`Unknown` after loops; `Empty` once the list is known to be empty)
    popped: Int = 0,
    // the local variables this boolean tests with `IsArray`
    isArrayOf: Set[Local] = Set(),
  ) extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean =
      srcs.isEmpty && locs.isEmpty && clos.isEmpty && cst == Cst.Bot &&
      rest.isEmpty

    /** join operator */
    def ⊔(that: AbsValue): AbsValue = AbsValue(
      srcs ++ that.srcs,
      locs ++ that.locs,
      clos ++ that.clos,
      cst ⊔ that.cst,
      rest ++ that.rest,
      maybeAbsent || that.maybeAbsent,
      joinPopped(popped, that.popped),
      // a test result survives only if both sides are the same test
      if (isBottom) that.isArrayOf
      else if (that.isBottom) isArrayOf
      else isArrayOf intersect that.isArrayOf,
    )

    def toTaint: AbsValue = AbsValue(srcs = srcs.map(_.toTaint), cst = Cst.Top)
    def present: AbsValue = copy(maybeAbsent = false)

    def canBeTrue: Boolean = cst match
      case Cst.Bot               => false
      case Cst.One(Lit.LBool(b)) => b
      case _                     => true
    def canBeFalse: Boolean = cst match
      case Cst.Bot               => false
      case Cst.One(Lit.LBool(b)) => !b
      case Cst.One(_)            => false
      case Cst.Top               => true

    /** drop information that is only meaningful inside one function */
    def stripKeys(dropWithout: Boolean = true): AbsValue =
      def without(s: Src) = dropWithout && s.path.last == Some(WithoutElem)
      def local(s: Src) = s.key.isDefined || without(s)
      if (!srcs.exists(local) && isArrayOf.isEmpty) this
      else
        val newSrcs = srcs.map { s =>
          val p = if (without(s)) s.path.init else s.path
          Src(p, s.value)
        }
        copy(srcs = newSrcs, isArrayOf = Set())

    /** the value known not to be an array */
    def nonArray: AbsValue = copy(srcs = srcs.map { s =>
      if (!s.value || s.path.last == Some(WithoutElem)) s
      else s.copy(path = s.path.add(WithoutElem), key = None)
    })

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = toString
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element: any value carrying no data of interest */
    lazy val Top: AbsValue = AbsValue(cst = Cst.Top)

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue()

    /** values carrying no data of interest */
    lazy val Clean: AbsValue = Top

    /** `popped` after loops */
    val Unknown = -1

    /** `popped` of lists known to be empty */
    val Empty = Int.MaxValue

    def joinPopped(a: Int, b: Int): Int =
      if (a == b || b == Empty) a else if (a == Empty) b else Unknown

    def apply(lit: Lit): AbsValue = AbsValue(cst = Cst.One(lit))

    def bool(t: Boolean, f: Boolean): AbsValue =
      if (t && f) Clean
      else if (t) AbsValue(Lit.LBool(true))
      else if (f) AbsValue(Lit.LBool(false))
      else Bot

    def src(path: APath, value: Boolean = true): AbsValue =
      AbsValue(srcs = Set(Src(path, value)), cst = Cst.Top)

    def alloc(site: String): AbsValue =
      AbsValue(locs = Set(Loc(site)), cst = Cst.Top)

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) =>
      import elem.*
      val parts = List(
        if (srcs.isEmpty) None
        else Some(srcs.map(s => s"$s").toList.sorted.mkString("srcs: {", ", ", "}")),
        if (locs.isEmpty) None
        else Some(locs.map(_.site).toList.sorted.mkString("locs: {", ", ", "}")),
        if (clos.isEmpty) None
        else Some(clos.map(_.fname).toList.sorted.mkString("clos: {", ", ", "}")),
        if (rest.isEmpty) None
        else Some(rest.toList.sorted.mkString("rest: {", ", ", "}")),
        Some(cst).collect { case Cst.One(l) => s"cst: $l" },
        Option.when(maybeAbsent)("maybe-absent"),
        Option.when(popped != 0)(s"popped: $popped"),
        if (isArrayOf.isEmpty) None
        else Some(isArrayOf.map(_.toString).toList.sorted.mkString("is-array: {", ", ", "}")),
      ).flatten
      if (elem.isBottom) app >> "⊥"
      else if (parts.isEmpty) app >> "⊤"
      else app >> parts.mkString("[", ", ", "]")
  }
}
