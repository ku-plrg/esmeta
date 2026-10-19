package esmeta.flowsummary

import esmeta.cfg.{Block, Call, Node}
import esmeta.ir.*
import esmeta.flowsummary.Lit.*

/** helpers for the flow-summary analysis that do not depend on its state */
object Semantics {

  /** locals mapped to abstract values */
  type Locals = Map[String, AbsVal]

  /** the evaluation context of an expression */
  case class Ctx(st: Locals, fname: String)

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
    "ToNumber", "ToNumeric", "ToIntegerOrInfinity", "ToLength", "ToIndex",
    "ToUint32", "ToInt32", "ToUint16", "ToInt16", "ToUint8", "ToInt8",
    "ToUint8Clamp", "ToBoolean", "ToBigInt", "ToBigInt64", "ToBigUint64",
    "LengthOfArrayLike", "HasProperty", "HasOwnProperty", "IsCallable",
    "IsConstructor", "IsArray", "IsRegExp", "IsIntegralNumber", "SameValue",
    "SameValueZero", "SameValueNonNumber", "IsStrictlyEqual", "IsLooselyEqual",
    "IsLessThan", "DeletePropertyOrThrow", "IsConcatSpreadable",
    "ValidateTypedArray", "TypedArrayLength", "IsTypedArrayOutOfBounds",
    "MakeTypedArrayWithBufferWitnessRecord", "IsDetachedBuffer",
    "StringIndexOf", "StringLastIndexOf", "IsStringWellFormedUnicode",
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

  /** join of locals; a local missing on one side may be absent */
  def joinLocals(x: Locals, y: Locals): Locals =
    (x.keySet ++ y.keySet).map { k =>
      (x.get(k), y.get(k)) match
        case (Some(u), Some(v)) => k -> (u ⊔ v)
        case (Some(u), None)    => k -> u.copy(maybeAbsent = true)
        case (None, Some(v))    => k -> v.copy(maybeAbsent = true)
        case _                  => ???
    }.toMap

  /** forget constants that keep changing in loops */
  def widen(prev: Locals, next: Locals): Locals = next.map { (k, v) =>
    prev.get(k) match
      case Some(pv) =>
        val c = if (pv.cst != v.cst) Cst.Top else v.cst
        val p = if (pv.popped != v.popped) AbsVal.Unknown else v.popped
        k -> v.copy(cst = c, popped = p)
      case _ => k -> v
  }

  /** forget constants of loop-carried variables up front */
  def preWiden(assigned: Set[String], st: Locals): Locals = st.map { (k, v) =>
    if (!assigned.contains(k)) k -> v
    else
      val c = if (v.cst == Cst.Bot) Cst.Bot else Cst.Top
      val p = if (v.locs.nonEmpty) AbsVal.Unknown else v.popped
      k -> v.copy(cst = c, popped = p)
  }

  /** locals assigned in a CFG node */
  def assignedVars(node: Node): Set[String] = node match
    case Block(_, insts, _) => insts.flatMap(assignedVars).toSet
    case call: Call         => Set(localName(call.lhs))
    case _                  => Set()

  def assignedVars(inst: NormalInst): Set[String] = inst match
    case ILet(x, _)                 => Set(x.name)
    case IAssign(x: Local, _)       => Set(localName(x))
    case IPop(x, ERef(l: Local), _) => Set(localName(x), localName(l))
    case IPop(x, _, _)              => Set(localName(x))
    case _                          => Set()

  /** drop information that is only meaningful inside one function */
  def stripKeys(v: AbsVal, dropWithout: Boolean = true): AbsVal =
    def without(s: Src) = dropWithout && s.path.last == Some(WithoutElem)
    def local(s: Src) = s.key.isDefined || without(s)
    if (!v.srcs.exists(local) && v.isArrayOf.isEmpty) v
    else
      val srcs = v.srcs.map { s =>
        val p = if (without(s)) s.path.init else s.path
        Src(p, s.value)
      }
      v.copy(srcs = srcs, isArrayOf = Set())

  /** CodeQL turns value flow that reads an unknown array index into taint
    * flow from the array itself (`TaintTrackingPrivate.qll`), but not reads of
    * a known index or `WithArrayElement`; add those taint flows explicitly.
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
  def abruptVars(cond: Expr): List[String] = cond match
    case EBinary(BOp.And, l, r) => abruptVars(l) ++ abruptVars(r)
    case ETypeCheck(ERef(x: Local), ty) if isAbrupt(ty) => List(localName(x))
    case EUnary(
          UOp.Not,
          EBinary(BOp.Eq, ERef(Field(x: Local, EStr("Type"))), EEnum("normal")),
        ) =>
      List(localName(x))
    case _ => Nil

  /** locals known to hold completion records (`assert (? x: Completion)`) */
  def compMark(x: String): String = s"#comp:$x"
  def isComp(ref: Ref)(using ctx: Ctx): Boolean = ref match
    case x: Local => ctx.st.contains(compMark(localName(x)))
    case _        => false
  def compChecked(expr: Expr): List[String] = expr match
    case EBinary(_, l, r) => compChecked(l) ++ compChecked(r)
    case EUnary(_, e)     => compChecked(e)
    case ETypeCheck(ERef(x: Local), ty) if ty.ty.isCompletion =>
      List(localName(x))
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

  /** a value known not to be an array */
  def nonArray(v: AbsVal): AbsVal = v.copy(srcs = v.srcs.map { s =>
    if (!s.value || s.path.last == Some(WithoutElem)) s
    else s.copy(path = s.path.add(WithoutElem), key = None)
  })

  /** the locals tested by `IsArray` in a condition, and whether the condition
    * holds when the value is an array
    */
  def isArrayTest(cond: Expr, st: Locals): Option[(Set[String], Boolean)] =
    def tested(x: Local) =
      st.get(localName(x)).map(_.isArrayOf).filter(_.nonEmpty)
    cond match
      case ERef(x: Local)                            => tested(x).map((_, true))
      case EBinary(BOp.Eq, ERef(x: Local), EBool(b)) => tested(x).map((_, b))
      case EUnary(UOp.Not, c) => isArrayTest(c, st).map((s, b) => (s, !b))
      case _                  => None

  /** refine locals for both branches of a condition (`None`: unreachable) */
  def refine(cond: Expr, st0: Locals): (Locals, Locals) =
    val st = st0 ++ compChecked(cond).map(compMark(_) -> AbsVal.Clean)
    val (t, e) = cond match
      // abrupt completions are not represented; the value is normal
      case _ if abruptVars(cond).nonEmpty =>
        (st ++ abruptVars(cond).map(_ -> AbsVal.Bot), st)
      case EBinary(BOp.Lt, EMath(n), ESizeOf(ERef(x: Local)))
          if n == 0 && st.contains(localName(x)) =>
        // the list is empty in the else branch
        val v = st(localName(x))
        (st, st + (localName(x) -> v.copy(rest = Set(), popped = AbsVal.Empty)))
      case _ => (st, st)
    isArrayTest(cond, st) match
      case Some((tested, positive)) =>
        def mark(s: Locals): Locals = s.map { (k, v) =>
          if (tested.contains(k)) k -> nonArray(v) else k -> v
        }
        if (positive) (t, mark(e)) else (mark(t), e)
      case None => (t, e)
}
