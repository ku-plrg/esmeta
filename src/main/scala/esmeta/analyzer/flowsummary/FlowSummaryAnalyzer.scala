package esmeta.analyzer.flowsummary

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.flowsummary.{Path => APath, *}
import esmeta.flowsummary.Key.*
import esmeta.flowsummary.Lit.*
import esmeta.flowsummary.Semantics.*
import esmeta.util.{Loc => _, *}
import esmeta.util.Appender.*
import scala.collection.mutable

/** Derives CodeQL flow summaries of a built-in function by abstract
  * interpretation of the CFG of ECMA-262.
  *
  * Abstract operations that inspect or build JS objects (`Get`, `Set`, `Call`,
  * `ArraySpeciesCreate`, ...) are modeled directly, so the analysis never
  * descends into object internal methods. Other abstract operations are
  * analyzed through their CFG with views on the abstract arguments. The heap is
  * flow-insensitive: control points that read a heap location are revisited
  * when it changes.
  */
class FlowSummaryAnalyzer(
  val cfg: CFG,
  val target: Func,
  val intent: Set[Intent] = Set(),
  val maxDepth: Int = 3,
) extends Analyzer
  with AbsTransferDecl {

  val log: Boolean = false
  def logging: Unit = ()

  // ---------------------------------------------------------------------------
  // views: calling contexts
  // ---------------------------------------------------------------------------
  enum View extends ViewLike:
    case Entry
    case Context(args: List[AbsVal], captured: Locals)
    case Merged // too many contexts: all others are joined
    def isEmpty: Boolean = this == Entry
    override def hashCode: Int =
      scala.util.hashing.MurmurHash3.productHash(this)

  def viewRule(detail: Boolean): Rule[View] = (app, view) =>
    view match
      case View.Entry      => app
      case View.Merged     => app >> "merged"
      case _: View.Context => app >> "ctx"

  val emptyView: View = View.Entry
  def getEntryView(view: View): View = view

  // ---------------------------------------------------------------------------
  // domains
  // ---------------------------------------------------------------------------
  case class AbsValue(v: AbsVal) extends AbsValueLike:
    def getString(state: AbsState): String = v.toString
  object AbsValue extends DomainLike[AbsValue]:
    val Top: AbsValue = AbsValue(AbsVal.Clean)
    val Bot: AbsValue = AbsValue(AbsVal.Bot)
    given rule: Rule[AbsValue] = (app, elem) => app >> elem.v.toString

  case class AbsState(reachable: Boolean, locals: Locals) extends AbsStateLike:
    def hasImprec: Boolean = false
    def isBottom: Boolean = !reachable
    def ⊔(that: AbsState): AbsState =
      if (!reachable) that
      else if (!that.reachable) this
      else AbsState(true, joinLocals(locals, that.locals))
    def define(x: String, v: AbsVal): AbsState =
      copy(locals = locals - compMark(x) + (x -> v))
  object AbsState extends DomainLike[AbsState]:
    val Top: AbsState = AbsState(true, Map())
    val Bot: AbsState = AbsState(false, Map())
    def apply(locals: Locals): AbsState = AbsState(true, locals)
    given rule: Rule[AbsState] = (app, elem) =>
      if (!elem.reachable) app >> "⊥"
      else app >> elem.locals.toList.sortBy(_._1).mkString("{", ", ", "}")

  case class AbsRet(ret: AbsVal) extends AbsRetLike:
    def value: AbsValue = AbsValue(ret)
  object AbsRet extends DomainLike[AbsRet]:
    val Top: AbsRet = AbsRet(AbsVal.Clean)
    val Bot: AbsRet = AbsRet(AbsVal.Bot)
    given rule: Rule[AbsRet] = (app, elem) => app >> elem.ret.toString

  case class Effect() extends EffectLike
  object Effect extends DomainLike[Effect]:
    val Top: Effect = Effect()
    val Bot: Effect = Effect()
    given rule: Rule[Effect] = (app, _) => app

  // ---------------------------------------------------------------------------
  // analysis state
  // ---------------------------------------------------------------------------
  /** flow-insensitive heap of objects allocated during the call */
  val heap = mutable.Map[Loc, Map[Key, AbsVal]]()

  /** control points that read each heap location */
  val readers = mutable.Map[Loc, Set[ControlPoint]]()

  /** calling contexts of each function */
  val contexts = mutable.Map[String, Set[View]]()

  val flows = mutable.Set[Flow]()
  val yets = mutable.Set[String]()
  val warnings = mutable.Set[String]()
  def warn(msg: String): Unit = warnings += msg

  /** the entry state of the built-in */
  private val entrySt: AbsState = AbsState(
    Map(
      "this" -> AbsVal.src(APath(Root.This)),
      "ArgumentsList" -> AbsVal(rest = Set(0), cst = Cst.Top),
      "NewTarget" -> AbsVal(LUndef),
    ),
  )
  npMap = Map(NodePoint(target, target.entry, emptyView) -> entrySt)

  // ---------------------------------------------------------------------------
  // implementation for the general analyzer
  // ---------------------------------------------------------------------------
  /** ids of views in the order they are created, for a total order */
  private val viewIds =
    mutable.Map[View, Int](View.Entry -> 0, View.Merged -> 1)
  def viewId(view: View): Int = viewIds.getOrElseUpdate(view, viewIds.size)

  /** a total order of control points, so that results do not depend on the
    * iteration order of sets
    */
  given cpOrder: Ordering[ControlPoint] = Ordering.by {
    case NodePoint(f, n, v) => (f.id, n.id, viewId(v))
    case ReturnPoint(f, v)  => (f.id, Int.MaxValue, viewId(v))
  }

  val worklist: Worklist[ControlPoint] =
    PriorityQueueWorklist(npMap.keySet)(using cpOrder)
  val transfer: AbsTransfer = new AbsTransfer

  def reachable(np: NodePoint[Node]): Boolean = getResult(np).reachable
  def reachable(rp: ReturnPoint): Boolean = rpMap.contains(rp)

  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String = cp match
    case np: NodePoint[_] => s"$cp -> ${getResult(np)}"
    case rp: ReturnPoint  => s"$cp -> ${getResult(rp)}"

  /** join a state into a node point, widening at loop heads */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, inSt) = pair
    if (inSt.reachable)
      val old = getResult(np)
      val newSt = np.node match
        case br: Branch if br.isLoop =>
          if (!old.reachable)
            AbsState(preWiden(loopAssigned(np.func, br), inSt.locals))
          else
            val joined = old ⊔ inSt
            AbsState(widen(old.locals, joined.locals))
        case _ => old ⊔ inSt
      if (newSt != old)
        npMap += np -> newSt
        worklist += np

  /** locals assigned in the body of a loop */
  private val loopAssignedMap = mutable.Map[Branch, Set[String]]()
  def loopAssigned(func: Func, br: Branch): Set[String] =
    loopAssignedMap.getOrElseUpdate(
      br, {
        // nodes on a cycle through the loop head
        val body = br.thenNode.fold(Set[Node]())(_.reachable)
        body.filter(_.reachable.contains(br)).flatMap(assignedVars)
      },
    )

  // ---------------------------------------------------------------------------
  // result
  // ---------------------------------------------------------------------------
  case class Summary(
    name: String,
    flows: Set[Flow],
    yets: Set[String],
    warnings: Set[String],
  )

  lazy val result: Summary =
    try analyze
    catch
      case BudgetExceeded =>
        warn("budget exceeded: partial result")
    getResult(ReturnPoint(target, emptyView)) match
      case AbsRet(ret) => transfer.flowsTo(ret, APath(Root.Ret), 0)
    Summary(
      target.name,
      subsume(
        if (intent.contains(Intent.ExplicitTaint)) implicitTaint(flows.toSet)
        else flows.toSet,
      ),
      yets.toSet,
      warnings.toSet,
    )
}
