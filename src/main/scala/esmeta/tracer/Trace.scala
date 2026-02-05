package esmeta.tracer

import esmeta.tracer.util.*
import esmeta.util.BaseUtils.raise
import esmeta.util.Locational
import io.circe.*
import io.circe.parser.decode

enum Trace extends Locational {
  // Return,
  // Throw,
  // With,
  case Binary(op: String, isThrown: Option[Boolean] = None)
  case BinaryPre(op: String)
  case Conditional(bool: Boolean)
  case Declare(name: String)
  // EndExecution,
  // EndExpression,
  case ForInObject(obj: String)
  case FunctionEnter(fo: String)
  case FunctionExit(fo: String)
  case GetField(
    prop: String,
    isInternal: Option[Boolean] = None,
    isGlobal: Option[Boolean] = None,
    isThrown: Option[Boolean] = None,
  )
  case GetFieldPre(
    prop: String,
    isInternal: Option[Boolean] = None,
    isGlobal: Option[Boolean] = None,
  )
  // InstrumentCode,
  // InstrumentCodePre,
  case InvokeFun(
    f: String,
    isInternal: Option[Boolean] = None,
    isThrown: Option[Boolean] = None,
  )
  case InvokeFunPre(f: String, isInternal: Option[Boolean] = None)
  case Literal(valueType: String)
  // OnReady,
  case PutField(prop: String, isThrown: Option[Boolean] = None)
  case PutFieldPre(prop: String)
  case Read(name: String)
  // RunInstrumentedFunctionBody,
  // ScriptEnter,
  // ScriptExit,
  case Unary(op: String, isThrown: Option[Boolean] = None)
  case UnaryPre(op: String)
  case Write(name: String)
  case Print(text: String)
  // not a jalangi hook but used to check `print` statements

  case Unpacked() // not a jalangi hook, but used to represent 'absence' of nested traces

  def matches(that: Trace): Boolean = this == that

  def isPre: Option[Boolean] = this match
    case _: BinaryPre     => Some(true)
    case _: UnaryPre      => Some(true)
    case _: GetFieldPre   => Some(true)
    case _: PutFieldPre   => Some(true)
    case _: InvokeFunPre  => Some(true)
    case _: FunctionEnter => Some(true)
    case _: Binary        => Some(false)
    case _: Unary         => Some(false)
    case _: GetField      => Some(false)
    case _: PutField      => Some(false)
    case _: InvokeFun     => Some(false)
    case _: FunctionExit  => Some(false)
    case _                => None

  def pairs(that: Trace): Boolean = (this, that) match
    case (_: Trace.BinaryPre, _: Trace.Binary)           => true
    case (_: Trace.UnaryPre, _: Trace.Unary)             => true
    case (_: Trace.GetFieldPre, _: Trace.GetField)       => true
    case (_: Trace.PutFieldPre, _: Trace.PutField)       => true
    case (_: Trace.InvokeFunPre, _: Trace.InvokeFun)     => true
    case (_: Trace.FunctionEnter, _: Trace.FunctionExit) => true
    case _                                               => false

}

object Trace {
  def from(str: String): Trace = {
    import JsonProtocol.given
    decode[Trace](str) match {
      case Right(trace) => trace
      case Left(err) =>
        raise(s"Failed to decode Trace from string: $str\nError: $err")
    }
  }
}
