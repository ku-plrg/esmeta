package esmeta.dump.visualizer

import esmeta.*
import esmeta.cfg.*
import esmeta.dump.visualizer.util.NameResolver
import esmeta.util.SystemUtils.*

object DumpSecIdToFuncInfo:

  /* section id of html */
  type SecId = String
  /* funcId, visual funcName */
  type SingleFuncInfo = (Int, String)
  /* funcId, visual funcName, fallback id (e.g. abstract closures) */
  type MergedFuncInfo = (Int, String, List[Int])

  def apply(cfg: CFG): Unit =
    given CFG = cfg

    val secIdToFuncInfo: Map[SecId, MergedFuncInfo] =
      cfg.funcs
        .flatMap[(SecId, SingleFuncInfo)](apply)
        .groupBy[SecId]((secId, _) => secId)
        .view
        .mapValues[List[SingleFuncInfo]](_.map((_, single) => single))
        .mapValues[Option[MergedFuncInfo]] { infos =>
          infos
            // prefer non-closure functions
            .find((funcId, _) =>
              val func = cfg.funcs(funcId)
              !func.isClo && !func.isCont,
            )
            // pick arbitrary
            .orElse(infos.headOption)
            .map((_, _, infos.map(_._1).distinct))
        }
        .collect[(SecId, MergedFuncInfo)] { case (k, Some(v)) => k -> v }
        .toMap[SecId, MergedFuncInfo]

    dumpJson(
      name = "secIdToFunc",
      data = secIdToFuncInfo,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/secIdToFunc.json",
      silent = true,
    )

  private def apply(func: Func)(using CFG): Option[(SecId, SingleFuncInfo)] =
    for {
      algo <- func.irFunc.algo
      sectionId = algo.elem.parent.id
    } yield {
      val secId =
        if (func.isSDO && func.sdoInfo.isDefined)
          s"$sectionId|${NameResolver.extractSDO(func.sdoInfo.get)}"
        else sectionId
      secId -> (func.id, NameResolver.convertFuncName(func))
    }
