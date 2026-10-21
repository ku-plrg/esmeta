package esmeta.analyzer.flowsummary

import esmeta.ir.Name
import esmeta.util.Appender.*

/** view abstraction */
trait ViewDecl { self: FlowSummaryAnalyzer =>

  /** calling contexts: the abstract arguments and captured values of a call */
  enum View extends ViewLike:
    case Entry
    case Context(args: List[AbsValue], captured: Map[Name, AbsValue])
    case Merged // too many contexts: all others are joined

    /** empty check */
    def isEmpty: Boolean = this == Entry

    // structural hashes: singleton enum cases otherwise use identity hashes,
    // which makes the iteration order of sets differ between runs
    override def hashCode: Int =
      scala.util.hashing.MurmurHash3.productHash(this)

  /** appender */
  def viewRule(detail: Boolean): Rule[View] = (app, view) =>
    view match
      case View.Entry  => app
      case View.Merged => app >> "merged"
      case View.Context(args, captured) =>
        if (!detail) app >> "ctx"
        else
          val cap = captured.toList.sortBy(_._1.toString).map((x, v) => s"$x -> $v")
          app >> args.mkString("ctx(", ", ", ")")
          if (cap.nonEmpty) app >> cap.mkString(" [", ", ", "]")
          app

  /** empty view */
  val emptyView: View = View.Entry

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
