package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** AST value types */
enum AstTy extends TyElem with Lattice[AstTy] {

  /** the top element */
  case Top

  /** a simple ast type with a set of nonterminal names */
  case Simple(set: Set[String])

  /** a detailed ast type with its type name and right-hand side index */
  case Detail(name: String, idx: Int)

  case Value(asts: Set[AstValue])

  import AstTy.*

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => AstTy): Boolean = (this eq that) || (
    (this, that) match
      case (_, Top)                           => true
      case (Top, _)                           => false
      case (Detail(l, lidx), Detail(r, ridx)) => l == r && lidx == ridx
      case (Simple(ls), Detail(_, _))         => ls.isEmpty
      case (Simple(ls), Simple(rs))           => ls subsetOf rs
      case (Detail(l, lidx), Simple(rs))      => rs contains l
      case (Value(lset), Value(rset))         => lset subsetOf rset
      case (Simple(ls), Value(_))             => ls.isEmpty
      case (Detail(_, _), Value(_))           => false
      case (Value(lset), Simple(rs)) => lset.map(_.ast.name).toSet subsetOf rs
      case (Value(lset), Detail(r, ridx)) =>
        lset.forall {
          case AstValue(ast) =>
            ast.name == r && ast.idx == ridx
        }
      case _ => false
  )

  /** union type */
  def ||(that: => AstTy): AstTy =
    if (this eq that) this
    else if (this <= that) that
    else if (that <= this) this
    else
      (this, that) match
        case (Top, _) | (_, Top)             => Top
        case (Simple(ls), Simple(rs))        => Simple(ls ++ rs)
        case (Simple(ls), Detail(name, idx)) => Simple(ls + name)
        case (Detail(name, idx), Simple(rs)) => Simple(rs + name)
        case (Detail(lname, lidx), Detail(rname, ridx)) =>
          if (lname == rname && lidx == ridx) Detail(lname, lidx)
          else Simple(Set(lname, rname))
        case (Value(lset), Value(rset)) => Value(lset ++ rset)

        case (Value(lset), Simple(rs)) if (rs.isEmpty) => Value(lset)
        case (Simple(ls), Value(rset)) if (ls.isEmpty) => Value(rset)

        case (Simple(ls), Value(rset)) => Simple(ls ++ rset.map(_.ast.name))
        case (Value(lset), Simple(rs)) => Simple(rs ++ lset.map(_.ast.name))
        case (Detail(name, idx), Value(rset)) =>
          Simple(Set(name) ++ rset.map(_.ast.name))
        case (Value(lset), Detail(name, idx)) =>
          Simple(lset.map(_.ast.name) + name)

  /** intersection type */
  def &&(that: => AstTy): AstTy =
    if (this eq that) this
    else if (this <= that) this
    else if (that <= this) that
    else
      (this, that) match
        case (Top, other)             => other
        case (other, Top)             => other
        case (Simple(ls), Simple(rs)) => Simple(ls intersect rs)
        case (Simple(ls), Detail(name, idx)) =>
          if (ls.contains(name)) Detail(name, idx) else Bot
        case (Detail(name, idx), Simple(rs)) =>
          if (rs.contains(name)) Detail(name, idx) else Bot
        case (Detail(lname, lidx), Detail(rname, ridx)) =>
          if (lname == rname && lidx == ridx) Detail(lname, lidx) else Bot
        case (Value(lset), Value(rset)) => Value(lset intersect rset)
        case (Simple(ls), Value(rset)) =>
          Value(rset.filter(ast => ls.contains(ast.ast.name)))
        case (Value(lset), Simple(rs)) =>
          Value(lset.filter(ast => rs.contains(ast.ast.name)))
        case (Detail(name, idx), Value(rset)) =>
          Value(rset.filter(ast => ast.ast.name == name && ast.ast.idx == idx))
        case (Value(lset), Detail(name, idx)) =>
          Value(lset.filter(ast => ast.ast.name == name && ast.ast.idx == idx))

  /** prune type */
  def --(that: => AstTy): AstTy =
    if (that.isBottom) this
    else
      (this, that) match
        case _ if this <= that                 => Bot
        case (Simple(lset), Simple(rset))      => Simple(lset -- rset)
        case (Simple(lset), Detail(name, idx)) => Simple(lset - name)
        case (Detail(lname, lidx), Detail(rname, ridx)) =>
          if (lname == rname && lidx == ridx) Bot else Detail(lname, lidx)
        case (Detail(name, idx), Simple(rset)) =>
          if (rset.contains(name)) Bot else Detail(name, idx)
        case (Value(lset), Value(rset)) => Value(lset -- rset)
        case (Value(lset), Simple(rset)) =>
          Value(lset.filterNot(ast => rset.contains(ast.ast.name)))
        case (Value(lset), Detail(name, idx)) =>
          Value(
            lset.filterNot(ast => ast.ast.name == name && ast.ast.idx == idx),
          )
        case _ => this

  /** get the set of type names */
  def names: BSet[String] = this match
    case Top             => Inf
    case Simple(names)   => Fin(names)
    case Detail(name, _) => Fin(Set(name))
    case Value(asts)     => Fin(asts.map(_.ast.name))

  /** AST containment check */
  def contains(value: AstValue): Boolean =
    val AstValue(ast) = value
    this match
      case Top               => true
      case Simple(names)     => names.exists(ast.types.contains)
      case Detail(name, idx) => ast.name == name && ast.idx == idx
      case Value(asts)       => asts.contains(value)

  /** to list of atomic AST types */
  def toAtomicTys: List[AstTy] = this match
    case Top               => List(Top)
    case Simple(names)     => names.toList.map(x => Simple(Set(x)))
    case Detail(name, idx) => List(Detail(name, idx))
    case Value(asts)       => asts.toList.map(ast => Value(Set(ast)))
}
object AstTy extends Parser.From(Parser.astTy) {
  lazy val Bot: Simple = Simple(Set())
}
