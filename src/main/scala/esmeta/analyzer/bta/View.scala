package esmeta.analyzer.bta

import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.Appender.*

/** view abstraction */
trait ViewDecl { self: BindingTimeAnalyzer =>

  /** view abstraction for analysis sensitivities */
  case class View() extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = ???
  }

  /** appender */
  def viewRule(detail: Boolean): Rule[View] = (app, view) => ???

  /** empty view */
  val emptyView: View = View()

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
