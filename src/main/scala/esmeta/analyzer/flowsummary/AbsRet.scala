package esmeta.analyzer.flowsummary

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: FlowSummaryAnalyzer =>

  case class AbsRet(value: AbsValue) extends AbsRetLike {

    /** bottom check */
    def isBottom: Boolean = value.isBottom

    /** join operator */
    def ⊔(that: AbsRet): AbsRet = AbsRet(this.value ⊔ that.value)
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet(AbsValue.Bot)

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) => app >> elem.value
  }
}
