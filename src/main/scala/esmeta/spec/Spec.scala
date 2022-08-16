package esmeta.spec

import esmeta.compiler.Compiler
import esmeta.lang.*
import esmeta.spec.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.Git
import esmeta.util.HtmlUtils.*
import esmeta.{error => ESMetaError, *}
import org.jsoup.nodes.Document

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[Git.Version] = None, // git version
  grammar: Grammar = Grammar(), // lexical/syntactic grammar productions
  algorithms: List[Algorithm] = Nil, // abstract algorithms for semantics
  tables: Map[String, Table] = Map(), // tables
  typeModel: TypeModel = TypeModel(), // type models
  document: Document = Document(""), // HTML Document element
) extends SpecElem {

  /** convert to an IR program */
  lazy val toIR: ir.Program = Compiler(this)

  /** convert to a control-flow graph (CFG) */
  lazy val toCFG: cfg.CFG = toIR.toCFG

  /** JavaScript parser */
  lazy val jsParser: parser.Parser = parser.Parser(grammar)

  /** get incomplete algorithms */
  lazy val incompleteAlgorithms: List[Algorithm] =
    algorithms.filter(!_.complete)

  /** get complete algorithms */
  lazy val completeAlgorithms: List[Algorithm] =
    algorithms.filter(_.complete)

  /** get all algorithm steps */
  lazy val allSteps: List[Step] = for {
    algo <- algorithms
    step <- algo.steps
  } yield step

  /** get incomplete algorithm steps */
  lazy val incompleteSteps: List[Step] =
    allSteps.filter(_.isInstanceOf[YetStep])

  /** get complete algorithm steps */
  lazy val completeSteps: List[Step] =
    allSteps.filter(!_.isInstanceOf[YetStep])

  /** mapping from algorithms names to algorithms */
  lazy val fnameMap: Map[String, Algorithm] =
    (for (algo <- algorithms) yield algo.head.fname -> algo).toMap

  /** get stats */
  lazy val stats: Stats = new Stats(this)

  /** get an algorithm by id attribute */
  def getAlgoById(id: String): Algorithm =
    algorithms.filter(_.elem.getId == id) match
      case algo :: Nil => algo
      case _           => error(s"no algorithms found for $id")
}
