package esmeta.analyzer.bta

import esmeta.ir.*
import esmeta.util.Appender.*

/** abstract states */
trait AbsStateDecl { self: BindingTimeAnalyzer =>
  case class AbsState(
    map: Map[Local, BindingTime],
  ) extends AbsStateLike {

    /** has imprecise elements */
    def hasImprec: Boolean = ???
  }
  object AbsState extends DomainLike[AbsState] {

    /** top element */
    def Top: AbsState = ???

    /** bottom element */
    def Bot: AbsState = ???

    /** appender */
    given rule: Rule[AbsState] = ???
  }

  enum BindingTime { case Static, Dynamic }
}
