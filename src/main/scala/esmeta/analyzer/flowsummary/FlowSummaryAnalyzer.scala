package esmeta.analyzer.flowsummary

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.flowsummary.{Path => APath, *}
import esmeta.flowsummary.Semantics.*
import esmeta.ir.{Local, Name}
import esmeta.util.{Loc => _, *}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
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
  val log: Boolean = false,
  val detail: Boolean = false,
  override val useRepl: Boolean = false,
  override val replContinue: Boolean = false,
) extends Analyzer
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with AbsTransferDecl
  with ViewDecl
  with EffectDecl {

  /** the entry state of the built-in */
  npMap = Map(
    NodePoint(target, target.entry, emptyView) -> AbsState(
      Map(
        Name("this") -> AbsValue.src(APath(Root.This)),
        Name("ArgumentsList") -> AbsValue(rest = Set(0), cst = Cst.Top),
        Name("NewTarget") -> AbsValue(Lit.LUndef),
      ),
    ),
  )

  // ---------------------------------------------------------------------------
  // Implementation for General Analyzer
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

  /** worklist of control points */
  val worklist: Worklist[ControlPoint] =
    PriorityQueueWorklist(npMap.keySet)(using cpOrder)

  /** abstract transfer function */
  val transfer: AbsTransfer = new AbsTransfer

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean = !getResult(np).isBottom

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean = !getResult(rp).isBottom

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String =
    val cpStr = cp.toString(detail = detail)
    val k = color.fold(cpStr)(setColor(_)(cpStr))
    cp match
      case np: NodePoint[_] => s"$k -> ${getResult(np)}"
      case rp: ReturnPoint  => s"$k -> ${getResult(rp)}"

  /** log directory */
  val logDir = s"$ANALYZE_LOG_DIR/flow-summary"

  /** logging the current analysis result */
  def logging: Unit =
    mkdir(logDir)
    val heapStrs = heap.toList.sortBy(_._1.site).map { (l, obj) =>
      val fields = obj.toList.map((k, v) => s"$k -> $v").sorted
      s"  ${l.site} -> " + fields.mkString("{", ", ", "}")
    }
    dumpFile(
      name = s"flow-summary analysis result of ${target.name}",
      data = (getStrings(detail = detail) ++ (
        if (heapStrs.isEmpty) Nil else "" :: "heap:" :: heapStrs
      )).mkString(LINE_SEP),
      filename = s"$logDir/${target.name}.log",
      silent = true,
    )

  // ---------------------------------------------------------------------------
  // Analysis State
  // ---------------------------------------------------------------------------
  /** flow-insensitive heap of objects allocated during the call */
  val heap = mutable.Map[Loc, Map[Key, AbsValue]]()

  /** control points that read each heap location */
  val readers = mutable.Map[Loc, Set[ControlPoint]]()

  /** calling contexts of each function */
  val contexts = mutable.Map[String, Set[View]]()

  val flows = mutable.Set[Flow]()
  val yets = mutable.Set[String]()
  val warnings = mutable.Set[String]()
  def warn(msg: String): Unit = warnings += msg

  /** join a state into a node point, widening at loop heads */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    if (!newSt.isBottom)
      val oldSt = getResult(np)
      if (!oldSt.isBottom && useRepl) Repl.merged = true
      val joined = np.node match
        case br: Branch if br.isLoop =>
          if (oldSt.isBottom) newSt.preWiden(loopAssigned(br))
          else (oldSt ⊔ newSt).widen(oldSt)
        case _ => oldSt ⊔ newSt
      if (joined != oldSt)
        npMap += np -> joined
        worklist += np

  /** locals assigned in the body of a loop */
  private val loopAssignedMap = mutable.Map[Branch, Set[Local]]()
  def loopAssigned(br: Branch): Set[Local] =
    loopAssignedMap.getOrElseUpdate(
      br, {
        // nodes on a cycle through the loop head
        val body = br.thenNode.fold(Set[Node]())(_.reachable)
        body.filter(_.reachable.contains(br)).flatMap(assignedVars)
      },
    )

  // ---------------------------------------------------------------------------
  // Result
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
    val AbsRet(ret) = getResult(ReturnPoint(target, emptyView))
    transfer.flowsTo(ret, APath(Root.Ret), 0)
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
