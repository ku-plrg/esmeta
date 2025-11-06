package esmeta.tracer

import esmeta.tracer.util.JsonProtocol.given
import esmeta.util.BaseUtils.raise
import esmeta.util.SystemUtils.*
import io.circe.parser.decode

object TraceRestore {
  def restore(
    targetTests: List[String],
  )(using workdir: WorkDir): CachedTraceMetadata = {
    val targetSet = targetTests.toSet
    val files = listFiles(workdir.path)
    val traces = for {
      file <- files
      name = file.getName
      abs = file.getAbsolutePath()
      if name.endsWith(".metadata")
      cached = decode[CachedTrace](readFile(abs)) match {
        case Right(trace) => trace
        case Left(error) =>
          raise(s"Failed to decode CachedTrace from $name: ${error}")
      }
      if targetSet.contains(cached.originalAbsPath)
    } yield cached
    CachedTraceMetadata(traces)
  }
}
