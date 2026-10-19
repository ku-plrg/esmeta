package esmeta.flowsummary

/** Prints derived flow summaries as a CodeQL library (`.qll`). */
object QLPrinter {

  /** how calls to a built-in look in JavaScript */
  enum Callee:
    case Method(name: String) // `x.name(...)`
    case Static(base: String, name: String) // `Base.name(...)`

    def className: String = this match
      case Method(n)    => s"EsmetaMethod_$n"
      case Static(b, n) => s"EsmetaStatic_${b}_$n"

    def summaryName: String = this match
      case Method(n)    => s"esmeta: .$n"
      case Static(b, n) => s"esmeta: $b.$n"

    def callType: String = this match
      case Method(_)    => "DataFlow::MethodCallNode"
      case Static(_, _) => "DataFlow::CallNode"

    def callCond: String = this match
      case Method(n) => s"""result.getMethodName() = "$n""""
      case Static(b, n) =>
        s"""result = DataFlow::globalVarRef("$b").getAMemberCall("$n")"""

  private val methodPattern =
    """INTRINSICS\.([A-Za-z]+)\.prototype\.([A-Za-z]+)""".r
  private val staticPattern = """INTRINSICS\.([A-Za-z]+)\.([A-Za-z]+)""".r

  /** the JavaScript callee of a built-in function name */
  def callee(fname: String): Option[Callee] = fname match
    case methodPattern(_, n) => Some(Callee.Method(n))
    case staticPattern(b, n) => Some(Callee.Static(b, n))
    case _                   => None

  /** pretty name of a built-in (`Array.prototype.flatMap`) */
  def prettyName(fname: String): String = fname.stripPrefix("INTRINSICS.")

  def apply(
    summaries: List[(String, Set[Flow])],
    header: String,
  ): String =
    val sb = new StringBuilder
    sb ++= s"""/**
              | * Flow summaries for ECMAScript built-in functions, derived from the
              | * specification by ESMeta. DO NOT EDIT.
              | *
              | * $header
              | */
              |overlay[local?]
              |module;
              |
              |private import javascript
              |private import semmle.javascript.dataflow.FlowSummary
              |""".stripMargin
    // methods of different prototypes share the same call shape
    val grouped = summaries
      .flatMap((f, flows) => callee(f).map(c => (c, f, flows)))
      .groupBy(_._1)
      .toList
      .sortBy(_._1.className)
    for ((c, entries) <- grouped) {
      val flows = entries.flatMap(_._3).toSet
      if (flows.nonEmpty)
        val origins = entries.map(e => prettyName(e._2)).sorted.mkString(", ")
        val alts = flows.toList
          .sortBy(f => (!f.value, f.input.toString, f.output.toString))
          .map { f =>
            s"""    preservesValue = ${f.value} and input = "${f.input}" and output = "${f.output}""""
          }
          .mkString("\n    or\n")
        sb ++= s"""
                  |/** Derived from: $origins */
                  |private class ${c.className} extends SummarizedCallable::Range {
                  |  ${c.className}() { this = "${c.summaryName}" }
                  |
                  |  override ${c.callType} getACallSimple() { ${c.callCond} }
                  |
                  |  override predicate propagatesFlow(string input, string output, boolean preservesValue) {
                  |$alts
                  |  }
                  |}
                  |""".stripMargin
    }
    sb.toString
}
