package esmeta.dump.visualizer

import esmeta.*
import esmeta.cfg.*
import esmeta.dump.visualizer.util.*
import esmeta.spec.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{Map as MMap, Set as MSet}

object DumpNodeIdToProgId {
  extension (str: String)
    def trimCode: String = str.trim().replace("\"use strict\";\n", "")

  type NodeIdToProgId = MMap[Int, MMap[String, MMap[String, (Int, Int)]]]

  val RECENT_DIR = s"$FUZZ_LOG_DIR/recent"

  def apply(cfg: CFG): Unit =
    given CFG = cfg

    val jsonProtocol = VisualizerJsonProtocol(cfg)
    import jsonProtocol.{*, given}
    val nvList =
      readJson[List[NodeViewInfoJson]](s"$RECENT_DIR/node-coverage.json")

    val nodeIdToProgId: NodeIdToProgId = MMap.empty
    val progIdSet: MSet[Int] = MSet.empty

    val total = nvList.length

    for ((nv, idx) <- nvList.zipWithIndex) do {
      val NodeViewInfoJson(_, NodeViewJson(node, view), scriptStr) = nv
      val script = scriptStr.toInt
      progIdSet += script

      val featIdToProgId = nodeIdToProgId.getOrElseUpdate(node.id, MMap.empty)
      val currentCode = readFile(s"$RECENT_DIR/minimal/$script.js").trimCode

      val (featId, pathStr) = view
        .map(v => (v.feature.id.toString, v.path.getOrElse("")))
        .getOrElse(("", ""))
      val stepCnt = StepCounter.count(currentCode, node.id, pathStr) {
        println(s"\r${idx + 1}/$total")
      }

      featIdToProgId
        .getOrElseUpdate(featId, MMap.empty)
        // update cpToProgId
        .getOrElseUpdate(pathStr, (script, stepCnt))

      view match
        case Some(ViewJson(enclosing, feature, path)) =>
          val min = featIdToProgId.getOrElseUpdate("minimal", MMap.empty)
          min.get("minimal") match
            case Some(scriptId, _) =>
              val prevMinimal = readFile(
                s"$RECENT_DIR/minimal/$scriptId.js",
              )
              val foundNewMinimal =
                prevMinimal.trimCode.length > currentCode.length
              if (foundNewMinimal) min += "minimal" -> (script, stepCnt)
            case None => min += "minimal" -> (script, stepCnt)
        case None =>
          featIdToProgId
            .getOrElseUpdate("minimal", MMap.empty)
            .getOrElseUpdate("minimal", (script, stepCnt))

    }

    for (progId <- progIdSet) do
      val codeWithOutUseStrict = readFile(
        s"$RECENT_DIR/minimal/$progId.js",
      ).trimCode
      dumpJson(
        name = s"progIdToScript for $progId",
        data = codeWithOutUseStrict,
        filename = s"$DUMP_VISUALIZER_LOG_DIR/progIdToScript/${progId}.json",
        silent = true,
      )

    for ((nodeId, featIdToProgId) <- nodeIdToProgId) do
      dumpJson(
        name = s"nodeIdToProgId for $nodeId",
        data = featIdToProgId,
        filename = s"$DUMP_VISUALIZER_LOG_DIR/nodeIdToProgId/${nodeId}.json",
        silent = true,
      )
}
