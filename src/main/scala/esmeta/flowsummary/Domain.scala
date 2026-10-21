package esmeta.flowsummary

/** Building blocks of the flow-summary analysis that do not depend on the
  * analyzer: access paths in the CodeQL summary syntax, abstract locations,
  * constants, heap keys, and derived flows. The abstract domains themselves are
  * defined in `esmeta.analyzer.flowsummary`.
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
    case LNum(d)  => Some(d)
    case LMath(n) => Some(n.toDouble)
    case LInf(pos) =>
      Some(if (pos) Double.PositiveInfinity else Double.NegativeInfinity)
    case _ => None

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

/** a derived flow: `input` flows to `output` */
case class Flow(input: Path, output: Path, value: Boolean):
  override def toString: String =
    val kind = if (value) "value" else "taint"
    s"$input -> $output ($kind)"
