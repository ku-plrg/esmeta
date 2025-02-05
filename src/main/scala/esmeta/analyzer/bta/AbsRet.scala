package esmeta.analyzer.bta

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: BindingTimeAnalyzer =>
  case class AbsRet(value: AbsValue) extends AbsRetLike {

    /** has imprecise elements */
    def hasImprec: Boolean = ???
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    def Top: AbsRet = ???

    /** bottom element */
    def Bot: AbsRet = ???

    /** appender */
    given rule: Rule[AbsRet] = ???
  }
}
