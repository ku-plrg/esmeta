package esmeta.analyzer.bta

import esmeta.util.Appender.*

/** abstract values */
trait AbsValueDecl { self: BindingTimeAnalyzer =>
  case class AbsValue() extends AbsValueLike {

    /** has imprecise elements */
    def hasImprec: Boolean = ???

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = ???
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    def Top: AbsValue = ???

    /** bottom element */
    def Bot: AbsValue = ???

    /** appender */
    given rule: Rule[AbsValue] = ???
  }
}
