package esmeta.tracer

import esmeta.*
import esmeta.tracer.util.Stringifier
import esmeta.util.BaseUtils.stringify

trait TracerElem {
  def prettyprint: String =
    val stringifier = Stringifier()
    import stringifier.given
    stringify(this)
}

extension (v: state.Value) {
  def asEnum: state.Enum = {
    v match
      case e: state.Enum => e
      case _ =>
        throw new Exception(s"Value $v is not an Enum value.")
  }

  def isComp(using st: state.State): Boolean = {
    ty.CompT.contains(v, st)
  }

  def isAddr: Boolean = {
    v match
      case _: esmeta.state.Addr => true
      case _                    => false
  }

  def isStr: Boolean = {
    v match
      case esmeta.state.Str(_) => true
      case _                   => false
  }

  def unpackIfCompletion(using st: state.State): state.Value =
    val valueField = for {
      start <- Some(()) // dummy to use for-comprehension
      if v.isAddr && v.isComp
      field <- st.get(v.asAddr, state.Str("Value")).toOption
    } yield field
    valueField.getOrElse(v)
}

extension (f: cfg.Func) {
  def blockNodes: Set[cfg.Block] = {
    f.nodes.collect {
      case b: cfg.Block => b
    }.toSet
  }

  def callNodes: Set[cfg.Call] = {
    f.nodes.collect {
      case c: cfg.Call => c
    }.toSet
  }

  def branchNodes: Set[cfg.Branch] = {
    f.nodes.collect {
      case b: cfg.Branch => b
    }.toSet
  }
}

extension (callInst: ir.CallInst) {
  def immediateNormalCallOpt: Option[ir.EClo] = {
    callInst match
      case ir.ICall(_, eclo @ ir.EClo(fname, _), _) => Some(eclo)
      case _                                        => None
  }

  def sdoCallOpt: Option[ir.ISdoCall] = {
    callInst match
      case sdo: ir.ISdoCall => Some(sdo)
      case _                => None
  }
}

extension [T](set: Set[T]) {
  def asSingleton: T = {
    assert(set.size == 1)
    set.head
  }
}

extension [A, B](f: A => B) {
  inline def >>>[C](g: B => C): A => C = f andThen g
}

extension [A](a: A) {
  inline def pipe[B](f: A => B): B = f(a)
  inline def tap(f: A => Unit): A = { f(a); a }
  inline def |>[B](f: A => B): B = a.pipe(f)
}
