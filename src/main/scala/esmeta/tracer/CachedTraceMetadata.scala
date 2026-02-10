package esmeta.tracer

case class CachedTraceMetadata(
  traces: List[CachedTrace],
)

case class CachedTrace(
  originalAliasName: String,
  absPath: String,
  originalAbsPath: String,
  kind: Set[TargetKind],
)

enum TargetKind {
  case Jalangi
  case NodeProf
  case DynaJS
}

object TargetKind {
  lazy val skipNodeProf = Set(TargetKind.Jalangi, TargetKind.DynaJS)
  lazy val all = Set(TargetKind.Jalangi, TargetKind.NodeProf, TargetKind.DynaJS)

  def from(s: String): Option[TargetKind] = s match
    case "jalangi"  => Some(TargetKind.Jalangi)
    case "nodeprof" => Some(TargetKind.NodeProf)
    case "dynajs"   => Some(TargetKind.DynaJS)
    case _          => None
}
