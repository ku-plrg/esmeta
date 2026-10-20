package esmeta.flowsummary

import esmeta.error.ESMetaError

/** Rules that shape the derived flows the way CodeQL developers write their
  * own summaries. The base analysis is sound without them; each rule makes a
  * summary more precise or adds the rows CodeQL's taint tracking relies on.
  */
enum Intent(val name: String, val desc: String):

  /** a value on the `IsArray(v) = false` branch is `v.WithoutArrayElement`
    * (the "splat" pattern of `flatMap`, `flat`, and `arrify`)
    */
  case WithoutElement
    extends Intent(
      "without-element",
      "values known not to be arrays become `WithoutArrayElement` inputs",
    )

  /** an element read and written with the same key is an index-preserving
    * copy: `X.WithArrayElement -> Y`
    */
  case WithElement
    extends Intent(
      "with-element",
      "index-preserving copies become `WithArrayElement` inputs",
    )

  /** CodeQL only turns reads of an unknown array index into taint steps, so
    * flows through `ArrayElement[n]` or `WithArrayElement` get an explicit
    * taint row from the container
    */
  case ExplicitTaint
    extends Intent(
      "explicit-taint",
      "add taint rows for `ArrayElement[n]` and `WithArrayElement` inputs",
    )

object Intent:
  val all: Set[Intent] = values.toSet

  /** parse `all`, `none`, or a list of rule names */
  def parse(names: List[String]): Set[Intent] = names match
    case List("all")  => all
    case List("none") => Set()
    case _ =>
      names.map(n =>
        values
          .find(_.name == n)
          .getOrElse(throw UnknownIntent(n)),
      ).toSet

  def show(intents: Set[Intent]): String =
    if (intents.isEmpty) "none"
    else values.filter(intents.contains).map(_.name).mkString(",")

  case class UnknownIntent(name: String)
    extends ESMetaError(
      s"unknown intent rule `$name` (expected all, none, or " +
      values.map(_.name).mkString(", ") + ")",
    )
