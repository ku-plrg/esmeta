package esmeta.flowsummary

import esmeta.cfg.{Block, Call, Node}
import esmeta.ir.*
import esmeta.flowsummary.Lit.*

/** helpers for the flow-summary analysis that do not depend on its domains */
object Semantics {

  object BudgetExceeded extends Exception("analysis budget exceeded")

  /** the maximum number of calling contexts per function */
  val maxContexts = 8

  /** the maximum number of visited control points per built-in */
  val maxIter = 2_000_000

  /** the maximum number of callback parameters to consider */
  val maxParams = 4

  /** abstract operations whose results carry no data of interest */
  val cleanOps: Set[String] = Set(
    "__NEW_ERROR_OBJ__",
    "ToNumber",
    "ToNumeric",
    "ToIntegerOrInfinity",
    "ToLength",
    "ToIndex",
    "ToUint32",
    "ToInt32",
    "ToUint16",
    "ToInt16",
    "ToUint8",
    "ToInt8",
    "ToUint8Clamp",
    "ToBoolean",
    "ToBigInt",
    "ToBigInt64",
    "ToBigUint64",
    "LengthOfArrayLike",
    "HasProperty",
    "HasOwnProperty",
    "IsCallable",
    "IsConstructor",
    "IsArray",
    "IsRegExp",
    "IsIntegralNumber",
    "SameValue",
    "SameValueZero",
    "SameValueNonNumber",
    "IsStrictlyEqual",
    "IsLooselyEqual",
    "IsLessThan",
    "DeletePropertyOrThrow",
    "IsConcatSpreadable",
    "ValidateTypedArray",
    "TypedArrayLength",
    "IsTypedArrayOutOfBounds",
    "MakeTypedArrayWithBufferWitnessRecord",
    "IsDetachedBuffer",
    "StringIndexOf",
    "StringLastIndexOf",
    "IsStringWellFormedUnicode",
  )

  val WithElem = "WithArrayElement"
  val WithoutElem = "WithoutArrayElement"
  val iterElem = "__elem__"
  val callsToPattern = "calls to _([A-Za-z][A-Za-z0-9]*)_".r
  val varPattern = "_([A-Za-z][A-Za-z0-9]*)_".r
  val assignPattern = "^(Let|Set) _([A-Za-z][A-Za-z0-9]*)_ (be|to)".r

  def localName(x: Local): String = x match
    case Name(n) => n
    case Temp(i) => s"%$i"

  /** locals assigned in a CFG node */
  def assignedVars(node: Node): Set[Local] = node match
    case Block(_, insts, _) => insts.flatMap(assignedVars).toSet
    case call: Call         => Set(call.lhs)
    case _                  => Set()

  def assignedVars(inst: NormalInst): Set[Local] = inst match
    case ILet(x, _)                 => Set(x)
    case IAssign(x: Local, _)       => Set(x)
    case IPop(x, ERef(l: Local), _) => Set(x, l)
    case IPop(x, _, _)              => Set(x)
    case _                          => Set()

  /** CodeQL turns value flow that reads an unknown array index into taint flow
    * from the array itself (`TaintTrackingPrivate.qll`), but not reads of a
    * known index or `WithArrayElement`; add those taint flows explicitly.
    */
  def implicitTaint(flows: Set[Flow]): Set[Flow] =
    flows ++ (for {
      f <- flows if f.value
      i = f.input.steps.indexWhere(s => s == WithElem || preciseElem.matches(s))
      if i >= 0
      prefix = Path(f.input.root, f.input.steps.take(i))
      if !flows.exists(g =>
        g.input == prefix.add("ArrayElement") && g.output == f.output,
      )
    } yield Flow(prefix, f.output, false))

  private val preciseElem = "ArrayElement\\[[0-9]+\\]".r
  private def generalize(p: Path): Path =
    p.copy(steps = p.steps.map(preciseElem.replaceAllIn(_, "ArrayElement")))
  private def isPrecise(f: Flow): Boolean =
    f.toString.matches(".*ArrayElement\\[[0-9]+\\].*")

  /** drop flows covered by other flows */
  def subsume(flows: Set[Flow]): Set[Flow] = flows.filterNot {
    // a precise array index already covered by the unknown index
    case f @ Flow(i, o, v)
        if (generalize(i) != i || generalize(o) != o) &&
        flows.exists(g =>
          g != f && g.value == v && !isPrecise(g) &&
          generalize(i) == generalize(g.input) &&
          generalize(o) == generalize(g.output),
        ) =>
      true
    case f if f.toString.contains("@@") => true // symbol-keyed dispatch
    case Flow(i, o, false) if flows.contains(Flow(i, o, true)) => true
    // `Argument[i]` already covered by `Argument[j..]`
    case f @ Flow(Path(Root.Arg(i), steps), _, _) =>
      flows.exists {
        case Flow(Path(Root.Rest(j), `steps`), f.output, v) =>
          j <= i && (v || !f.value)
        case _ => false
      }
    case _ => false
  }

  /** variables that are abrupt completions if the condition holds */
  def abruptVars(cond: Expr): List[Local] = cond match
    case EBinary(BOp.And, l, r) => abruptVars(l) ++ abruptVars(r)
    case ETypeCheck(ERef(x: Local), ty) if isAbrupt(ty) => List(x)
    case EUnary(
          UOp.Not,
          EBinary(BOp.Eq, ERef(Field(x: Local, EStr("Type"))), EEnum("normal")),
        ) =>
      List(x)
    case _ => Nil

  /** locals checked to be completion records in a condition */
  def compChecked(expr: Expr): List[Local] = expr match
    case EBinary(_, l, r) => compChecked(l) ++ compChecked(r)
    case EUnary(_, e)     => compChecked(e)
    case ETypeCheck(ERef(x: Local), ty) if ty.ty.isCompletion =>
      List(x)
    case _ => Nil

  def isAbrupt(ty: Type): Boolean =
    val s = ty.ty.toString
    s == "Abrupt" || s == "Throw"

  /** whether two contents may denote the same property */
  def mayAlias(a: String, b: String): Boolean =
    def isElem(c: String) = c.startsWith("ArrayElement")
    a == b || a == "AnyMember" || b == "AnyMember" ||
    (isElem(a) && isElem(b) && (a == "ArrayElement" || b == "ArrayElement"))

  def isIndex(s: String): Boolean = s.nonEmpty && s.forall(_.isDigit)

  def litToStr(lit: Lit): Option[String] = lit match
    case LStr(s) => Some(s)
    case LNum(d) =>
      Some(if (d.isWhole && !d.isInfinite) d.toLong.toString else d.toString)
    case LMath(n) => Some(n.toString)
    case LBool(b) => Some(b.toString)
    case LUndef   => Some("undefined")
    case LNull    => Some("null")
    case _        => None
}
