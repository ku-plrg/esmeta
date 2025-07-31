package esmeta.verify

import esmeta.*
import esmeta.cfg.*

class FindSdo(
  cfg: CFG,
  target: Func,
  initialThisValue: es.Ast,
) extends analyzer.tychecker.TyChecker(
    cfg,
    targetPattern = None,
    inferTypeGuard = true,
    typeSens = false,
    config = analyzer.tychecker.TyChecker.Config(),
    ignore = analyzer.tychecker.TyChecker.Ignore(),
    log = true,
    detail = false,
    silent = false,
    useRepl = true,
    replContinue = false,
  ) {
  override def targetFuncs: List[Func] = List(target)

  /** get initial abstract states in each node point */
  override protected def getInitNpMap(
    targets: List[Func],
  ): Map[NodePoint[Node], AbsState] = (for {
    func <- targets
    entry = func.entry
    (view, st) <- getViewWithSt(func)
    np = NodePoint(func, entry, view)
  } yield np -> (st)).toMap

  override protected def getViewWithSt(func: Func): List[(View, AbsState)] =
    val pairs = func.params.map {
      case ir.Param(x, ty, _, _) => x -> ty.ty.toValue
    }
    val locals = pairs
      .map { (x, v) => x -> AbsValue(v) }
      .map { (x, v) =>
        x -> (if (x == ir.Name("this"))
                AbsValue(
                  ty.ValueTy(ast =
                    ty.AstTy.Value(Set(state.AstValue(initialThisValue))),
                  ),
                )
              else v)
      }
    val view = if (typeSens) View(pairs.map(_._2)) else emptyView
    List(view -> getCalleeState(AbsState.Empty, locals))
}
