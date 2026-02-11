package esmeta.tracer

import esmeta.tracer.visitor.*

object Ignore {
  def apply(diffTree: TraceDiff.DiffTree)(using
    kind: TargetKind,
  ): TraceDiff.DiffTree =
    kind match
      case TargetKind.Jalangi  => forJalangi(diffTree)
      case TargetKind.NodeProf => forNodeProf(diffTree)
      case TargetKind.DynaJS   => forDynaJS(diffTree)

  def forJalangi(diffTree: TraceDiff.DiffTree): TraceDiff.DiffTree = {
    given TargetKind = TargetKind.Jalangi
    val ignore =
      SuppressInternalOperation().traverse >>>
      GetFromGlobal().traverse >>>
      RightSideFunctionLiteral().traverse >>>
      RightSideDeclareThenFunctionLiteral().traverse >>>
      RightSideThrown().traverse >>>
      ReadEval().traverse >>>
      SuppressOperator().traverse >>>
      LiteralAsRead().traverse >>>
      SuppressInternalOperationWithFE().traverse >>>
      FirstDeclare().traverse >>>
      RightSidePutField().traverse >>>
      RightSideDeclare().traverse >>>
      IncDecOperators().traverse
    TraceDiff.DiffTree(ignore(diffTree.roots))
  }

  def forNodeProf(diffTree: TraceDiff.DiffTree): TraceDiff.DiffTree = {
    given TargetKind = TargetKind.NodeProf
    val ignore =
      NodeProfLocate().traverse >>>
      GetFromGlobal().traverse >>>
      LiteralAsRead().traverse
    TraceDiff.DiffTree(ignore(diffTree.roots))
  }

  def forDynaJS(diffTree: TraceDiff.DiffTree): TraceDiff.DiffTree = {
    given TargetKind = TargetKind.DynaJS
    val ignore =
      SuppressInternalOperation().traverse >>>
      GetFromGlobal().traverse >>>
      RightSideFunctionLiteral().traverse >>>
      RightSideDeclareThenFunctionLiteral().traverse >>>
      RightSideThrown().traverse >>>
      ReadEval().traverse >>>
      SuppressOperator().traverse >>>
      LiteralAsRead().traverse >>>
      SuppressInternalOperationWithFE().traverse >>>
      FirstDeclare().traverse >>>
      RightSidePutField().traverse >>>
      RightSideDeclare().traverse // >>>
    // IncDecOperators().traverse
    TraceDiff.DiffTree(ignore(diffTree.roots))
  }
}
