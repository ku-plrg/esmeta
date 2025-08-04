package esmeta.dump.visualizer

import esmeta.*
import esmeta.cfg.*
import esmeta.dump.visualizer.util.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{ListBuffer, Map as MMap, Set as MSet}

object DumpNodeIdToTest262 {
  type FeatIdToProgId = MMap[String, MMap[String, String]]
  type NodeIdToTest262 = MMap[Int, FeatIdToProgId]

  val RECENT_DIR = s"$TEST262TEST_LOG_DIR/recent"
  val NODE_COVERAGE_JSON_PATH = s"$RECENT_DIR/node-coverage.json"

  def apply(cfg: CFG): Unit =
    val jsonProtocol = VisualizerJsonProtocol(cfg)
    import jsonProtocol.{*, given}

    val nodeIdToTest262: NodeIdToTest262 = MMap.empty

    val nvList = readJson[List[NodeViewInfoJson]](NODE_COVERAGE_JSON_PATH)

    for ((json) <- nvList) do {
      val NodeViewInfoJson(_, NodeViewJson(node, view), encoded) = json
      val (featureId, path) = view
        .map(viewJson =>
          val ViewJson(_, feature, path) = viewJson
          (feature.id.toString, path.getOrElse("")),
        )
        .getOrElse(("", ""))
      nodeIdToTest262
        .getOrElseUpdate(node.id, MMap.empty)
        .getOrElseUpdate(featureId, MMap.empty)
        .getOrElseUpdate(path, encoded)
    }

    for ((nodeId, featIdToProgId) <- nodeIdToTest262) do
      dumpJson(
        name = s"nodeIdToTest262 for $nodeId",
        data = featIdToProgId,
        filename = s"$DUMP_VISUALIZER_LOG_DIR/nodeIdToTest262/${nodeId}.json",
        silent = true,
      )
}
