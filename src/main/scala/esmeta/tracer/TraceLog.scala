package esmeta.tracer

case class TraceLog(
  traces: Vector[Trace] = Vector.empty,
  // TODO
  // containsEval: Boolean = false,
) {

  override def toString: String =
    traces.map(_.toString).mkString("", "\n", "\n")
}
