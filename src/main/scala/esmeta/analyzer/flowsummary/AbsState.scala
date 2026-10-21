package esmeta.analyzer.flowsummary

import esmeta.flowsummary.*
import esmeta.ir.Local
import esmeta.util.Appender.*

/** abstract states */
trait AbsStateDecl { self: FlowSummaryAnalyzer =>

  /** `comps` are the locals known to hold completion records
    * (`assert (? x: Completion)`); abrupt completions are not represented
    */
  case class AbsState(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    comps: Set[Local] = Set(),
  ) extends AbsStateLike {
    import AbsState.*

    /** bottom check */
    def isBottom: Boolean = !reachable

    /** has imprecise elements */
    def hasImprec: Boolean = false

    /** join operator; a local missing on one side may be absent */
    def ⊔(that: AbsState): AbsState =
      if (this.isBottom) that
      else if (that.isBottom) this
      else
        val newLocals = (locals.keySet ++ that.locals.keySet).map { x =>
          (locals.get(x), that.locals.get(x)) match
            case (Some(u), Some(v)) => x -> (u ⊔ v)
            case (Some(u), None)    => x -> u.copy(maybeAbsent = true)
            case (None, Some(v))    => x -> v.copy(maybeAbsent = true)
            case _                  => ???
        }.toMap
        AbsState(true, newLocals, comps ++ that.comps)

    /** widening: forget constants that keep changing in loops */
    def widen(prev: AbsState): AbsState = copy(locals = locals.map { (x, v) =>
      prev.locals.get(x) match
        case Some(pv) =>
          val c = if (pv.cst != v.cst) Cst.Top else v.cst
          val p = if (pv.popped != v.popped) AbsValue.Unknown else v.popped
          x -> v.copy(cst = c, popped = p)
        case _ => x -> v
    })

    /** forget constants of loop-carried variables up front */
    def preWiden(assigned: Set[Local]): AbsState =
      copy(locals = locals.map { (x, v) =>
        if (!assigned.contains(x)) x -> v
        else
          val c = if (v.cst == Cst.Bot) Cst.Bot else Cst.Top
          val p = if (v.locs.nonEmpty) AbsValue.Unknown else v.popped
          x -> v.copy(cst = c, popped = p)
      })

    /** getters */
    def get(x: Local): Option[AbsValue] = locals.get(x)
    def isComp(x: Local): Boolean = comps.contains(x)

    /** define a local */
    def define(x: Local, v: AbsValue): AbsState =
      copy(locals = locals + (x -> v), comps = comps - x)

    /** update locals */
    def update(f: Map[Local, AbsValue] => Map[Local, AbsValue]): AbsState =
      copy(locals = f(locals))

    /** mark locals as completion records */
    def markComps(xs: Iterable[Local]): AbsState = copy(comps = comps ++ xs)
  }
  object AbsState extends DomainLike[AbsState] {

    /** top element */
    lazy val Top: AbsState = AbsState(true, Map())

    /** bottom element */
    lazy val Bot: AbsState = AbsState(false, Map())

    /** constructors */
    def apply(locals: Map[Local, AbsValue]): AbsState = AbsState(true, locals)

    /** appender */
    given rule: Rule[AbsState] = (app, elem) =>
      if (elem.isBottom) app >> "⊥"
      else
        val locals = elem.locals.toList.sortBy(_._1.toString).map { (x, v) =>
          val mark = if (elem.isComp(x)) " (completion)" else ""
          s"$x -> $v$mark"
        }
        app >> locals.mkString("{", ", ", "}")
  }
}
