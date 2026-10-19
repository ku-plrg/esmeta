package esmeta.flowsummary

/** Abstract domain for deriving CodeQL flow summaries from the IR of built-in
  * functions.
  *
  * A value is described by where it may come from (`srcs`, access paths rooted
  * at the receiver or an argument), which objects allocated during the call it
  * may point to (`locs`), which abstract closures it may be (`clos`), and an
  * optional constant used to prune infeasible branches.
  */

/** roots of access paths */
enum Root:
  case This
  case Arg(idx: Int)
  case Rest(from: Int)
  case Ret // only for outputs

  // structural hashes: singleton enum cases otherwise use identity hashes,
  // which makes the iteration order of sets differ between runs
  override def hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  override def toString: String = this match
    case This    => "Argument[this]"
    case Arg(i)  => s"Argument[$i]"
    case Rest(i) => s"Argument[$i..]"
    case Ret     => "ReturnValue"

/** access paths in the CodeQL summary syntax (`Argument[0].ArrayElement`) */
case class Path(root: Root, steps: List[String] = Nil):
  def add(step: String): Path = Path(root, steps :+ step)
  def last: Option[String] = steps.lastOption
  def init: Path = Path(root, steps.dropRight(1))
  def depth: Int = steps.length
  override def toString: String = (root.toString :: steps).mkString(".")

/** a source of a value; `value = false` means taint (not value-preserving) */
case class Src(path: Path, value: Boolean, key: Option[String] = None):
  def toTaint: Src = copy(value = false, key = None)

/** abstract locations of objects, lists, and records allocated in a call */
case class Loc(site: String)

/** abstract closures with captured values */
case class Clo(fname: String, captured: Map[String, AbsVal])

/** literals tracked for branch pruning */
enum Lit:
  case LStr(s: String)
  case LNum(d: Double)
  case LMath(n: BigDecimal)
  case LInf(pos: Boolean)
  case LBool(b: Boolean)
  case LEnum(name: String)
  case LSym(name: String) // well-known symbols
  case LOpaque // property keys not known statically
  case LUndef, LNull

  // structural hashes: singleton enum cases otherwise use identity hashes,
  // which makes the iteration order of sets differ between runs
  override def hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  def toDouble: Option[Double] = this match
    case LNum(d)   => Some(d)
    case LMath(n)  => Some(n.toDouble)
    case LInf(pos) => Some(if (pos) Double.PositiveInfinity else Double.NegativeInfinity)
    case _         => None

/** flat lattice of constants */
enum Cst:
  case Bot
  case One(lit: Lit)
  case Top

  // structural hashes: singleton enum cases otherwise use identity hashes,
  // which makes the iteration order of sets differ between runs
  override def hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  def ⊔(that: Cst): Cst = (this, that) match
    case (Bot, c)                   => c
    case (c, Bot)                   => c
    case (One(a), One(b)) if a == b => this
    case _                          => Top

/** keys of abstract heap entries */
enum Key:
  case KIdx(idx: Int) // IR list element at a known index
  case KAny // IR list element at an unknown index
  case KField(name: String) // IR record field
  case KProp(content: String) // JS property, as a CodeQL content

  // structural hashes: singleton enum cases otherwise use identity hashes,
  // which makes the iteration order of sets differ between runs
  override def hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

/** abstract values */
case class AbsVal(
  srcs: Set[Src] = Set(),
  locs: Set[Loc] = Set(),
  clos: Set[Clo] = Set(),
  cst: Cst = Cst.Bot,
  rest: Set[Int] = Set(), // the remaining `ArgumentsList` from this offset
  maybeAbsent: Boolean = false,
  // elements popped from the front of the lists in `locs`
  // (`Unknown` after loops; `Empty` once the list is known to be empty)
  popped: Int = 0,
  // the local variables this boolean tests with `IsArray`
  isArrayOf: Set[String] = Set(),
):
  def ⊔(that: AbsVal): AbsVal = AbsVal(
    srcs ++ that.srcs,
    locs ++ that.locs,
    clos ++ that.clos,
    cst ⊔ that.cst,
    rest ++ that.rest,
    maybeAbsent || that.maybeAbsent,
    AbsVal.joinPopped(popped, that.popped),
    // a test result survives only if both sides are the same test
    if (isBottom) that.isArrayOf
    else if (that.isBottom) isArrayOf
    else isArrayOf intersect that.isArrayOf,
  )
  def isBottom: Boolean =
    srcs.isEmpty && locs.isEmpty && clos.isEmpty && cst == Cst.Bot &&
    rest.isEmpty
  def toTaint: AbsVal = AbsVal(srcs = srcs.map(_.toTaint), cst = Cst.Top)
  def present: AbsVal = copy(maybeAbsent = false)
  def canBeTrue: Boolean = cst match
    case Cst.Bot                 => false
    case Cst.One(Lit.LBool(b))   => b
    case _                       => true
  def canBeFalse: Boolean = cst match
    case Cst.Bot                 => false
    case Cst.One(Lit.LBool(b))   => !b
    case Cst.One(_)              => false
    case Cst.Top                 => true
object AbsVal:
  val Unknown = -1
  val Empty = Int.MaxValue
  def joinPopped(a: Int, b: Int): Int =
    if (a == b || b == Empty) a else if (a == Empty) b else Unknown
  val Bot: AbsVal = AbsVal()
  val Clean: AbsVal = AbsVal(cst = Cst.Top)
  def apply(lit: Lit): AbsVal = AbsVal(cst = Cst.One(lit))
  def bool(t: Boolean, f: Boolean): AbsVal =
    if (t && f) Clean
    else if (t) AbsVal(Lit.LBool(true))
    else if (f) AbsVal(Lit.LBool(false))
    else Bot
  def src(path: Path, value: Boolean = true): AbsVal =
    AbsVal(srcs = Set(Src(path, value)), cst = Cst.Top)

/** a derived flow: `input` flows to `output` */
case class Flow(input: Path, output: Path, value: Boolean):
  override def toString: String =
    val kind = if (value) "value" else "taint"
    s"$input -> $output ($kind)"
