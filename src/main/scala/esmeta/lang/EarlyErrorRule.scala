package esmeta.lang

sealed trait EarlyErrorRule extends Syntax {

  /** check whether it is complete */
  def complete: Boolean = this match
    case _: YetRule => false
    case _          => true
}

// static semantic : early errors
case class YetRule(yet: YetExpression) extends EarlyErrorRule

case class SyntaxErrorRule(cond: Condition, early: Boolean)
  extends EarlyErrorRule

case class MustCoverRule(covering: Literal, covered: Literal)
  extends EarlyErrorRule
