package esmeta.phase

import esmeta.*
import esmeta.analyzer.flowsummary.FlowSummaryAnalyzer
import esmeta.cfg.CFG
import esmeta.flowsummary.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `gen-flow-summary` phase */
case object GenFlowSummary extends Phase[CFG, Unit] {
  val name = "gen-flow-summary"
  val help = "derives CodeQL flow summaries of built-in functions."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val target = config.target.r
    val builtins = cfg.funcs
      .filter(f => f.isBuiltin && target.matches(f.name))
      .sortBy(_.name)
    val results = for (f <- builtins) yield
      val analyzer = FlowSummaryAnalyzer(
        cfg,
        f,
        config.intent,
        log = config.log,
        detail = config.detail,
        useRepl = config.useRepl,
        replContinue = config.replContinue,
      )
      val res =
        try analyzer.result
        catch
          case e: Throwable =>
            analyzer.Summary(f.name, Set(), Set(), Set(s"crashed: $e"))
      if (!cmdConfig.silent)
        println(s"[${res.flows.size}] ${QLPrinter.prettyName(f.name)}")
      res

    val out = config.out
    val version = cfg.spec.version.fold("unknown")(_.toString)
    dumpFile(
      name = "flow summaries (QL)",
      data = QLPrinter(
        results.map(r => r.name -> r.flows),
        s"ECMA-262 version: $version, CodeQL intent rules: " +
        Intent.show(config.intent),
      ),
      filename = s"$out/EsmetaBuiltins.qll",
    )
    dumpFile(
      name = "flow summaries (TSV)",
      data = (for {
        r <- results
        f <- r.flows.toList.sortBy(_.toString)
      } yield List(QLPrinter.prettyName(r.name), f.input, f.output, f.value)
        .mkString("\t")).mkString(LINE_SEP),
      filename = s"$out/summaries.tsv",
    )
    dumpFile(
      name = "analysis notes",
      data = (s"CodeQL intent rules: ${Intent.show(config.intent)}" :: (for {
        r <- results
        if r.yets.nonEmpty || r.warnings.nonEmpty
      } yield (s"## ${QLPrinter.prettyName(r.name)}" ::
      r.yets.toList.sorted.map(y => s"- yet: $y") :::
      r.warnings.toList.sorted.map(w => s"- warning: $w"))
        .mkString(LINE_SEP))).mkString(LINE_SEP + LINE_SEP),
      filename = s"$out/notes.md",
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = s),
      "regex of IR names of built-in functions to analyze.",
    ),
    (
      "out",
      StrOption((c, s) => c.out = s),
      "output directory.",
    ),
    (
      "intent",
      StrListOption((c, l) => c.intent = Intent.parse(l)),
      "CodeQL intent rules to apply: `all`, `none` (default), or a list of " +
      Intent.values.map(i => s"`${i.name}` (${i.desc})").mkString(", ") + ".",
    ),
    (
      "repl",
      BoolOption(_.useRepl = _),
      "use a REPL for the analysis of each built-in function.",
    ),
    (
      "repl-continue",
      BoolOption(_.replContinue = _),
      "run the `continue` command at startup when using the REPL.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "logging mode with detailed information.",
    ),
  )
  case class Config(
    var target: String =
      """INTRINSICS\.(Array|String)\.prototype\.[A-Za-z]+|INTRINSICS\.(Array|Object|JSON|String)\.[A-Za-z]+""",
    var out: String = s"$LOG_DIR/flow-summary",
    var intent: Set[Intent] = Set(),
    var useRepl: Boolean = false,
    var replContinue: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
  )
}
