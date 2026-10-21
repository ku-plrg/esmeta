package esmeta.analyzer.flowsummary

import esmeta.util.Appender.*

/** effects: not tracked, as the heap is flow-insensitive */
trait EffectDecl { self: FlowSummaryAnalyzer =>

  case class Effect() extends EffectLike
  object Effect extends DomainLike[Effect] {

    /** top element */
    lazy val Top: Effect = Effect()

    /** bottom element */
    lazy val Bot: Effect = Effect()

    /** appender */
    given rule: Rule[Effect] = (app, _) => app
  }
}
