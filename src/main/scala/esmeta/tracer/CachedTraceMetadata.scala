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
}

object TargetKind {
  lazy val jalangiOnly = Set(TargetKind.Jalangi)
  lazy val all = Set(TargetKind.Jalangi, TargetKind.NodeProf)
}
