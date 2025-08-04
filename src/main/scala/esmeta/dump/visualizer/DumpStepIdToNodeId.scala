package esmeta.dump.visualizer

import esmeta.DUMP_VISUALIZER_LOG_DIR
import esmeta.cfg.*
import esmeta.ir.{Func as IRFunc, Type as IRType, *}
import esmeta.ty.CompT
import esmeta.util.{given_Ordering_Pos, *}
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map as MMap, Set as MSet}

class DumpStepToNodeId private (func: Func) {
  val stepToNodeId: MMap[String, MSet[Int]] = MMap.empty
  val abruptMap: MMap[String, MMap[Pos, Int]] = MMap.empty

  val visited: MSet[Int] = MSet.empty

  lazy val compute = walk(func.entry, None)

  private def walk(
    node: Node,
    prevLocOpt: Option[Loc],
  ): Unit =

    def add(curLocOpt: Option[Loc], prevLocOpt: Option[Loc]): Unit =
      visited += node.id
      (node.loc, prevLocOpt) match
        case (None, _) =>
        case (Some(curLoc), None) =>
          stepToNodeId.safeAdd(curLoc.stepString, node.id)
        case (Some(curLoc), Some(prevLoc)) =>
          if (curLoc.steps != prevLoc.steps)
            stepToNodeId.safeAdd(curLoc.stepString, node.id)

    if (!visited.contains(node.id)) {
      node match
        case Block(_, insts, next) =>
          var plo = prevLocOpt
          insts.foreach((inst) => {
            add(inst.loc, plo)
            plo = inst.loc
          })
          next.foreach(walk(_, node.loc))
        case Call(_, _, next) =>
          add(node.loc, prevLocOpt)
          next.foreach(walk(_, node.loc))
        case Branch(_, _, cond, isAbruptNode, thenNode, elseNode) =>
          if (isAbruptNode && node.loc.isDefined && thenNode.isDefined)
            abruptMap.getOrElseUpdate(
              node.loc.get.stepString,
              MMap(),
            ) += (node.loc.get.start -> thenNode.get.id)
          else if (!isCompTCheck(cond)) {
            thenNode.foreach(tn =>
              (node.loc, tn.loc) match
                case (Some(l1), Some(l2)) if (l1.stepString == l2.stepString) =>
                  stepToNodeId.safeAdd(l1.stepString + "|if", node.id)
                  stepToNodeId.safeAdd(l2.stepString + "|then", tn.id)
                case _ => (),
            )

            elseNode.foreach(en =>
              (node.loc, en.loc) match
                case (Some(l1), Some(l2)) if (l1.stepString == l2.stepString) =>
                  stepToNodeId.safeAdd(l1.stepString + "|if", node.id)
                  stepToNodeId.safeAdd(l2.stepString + "|else", en.id)
                case _ => (),
            )
          }

          add(node.loc, prevLocOpt)
          thenNode.foreach(walk(_, node.loc))
          elseNode.foreach(walk(_, node.loc))
    }

    def isCompTCheck(cond: Expr): Boolean = cond match
      case ETypeCheck(_, IRType(CompT, None)) => true
      case _                                  => false
}

object DumpStepToNodeId {
  def apply(cfg: CFG): Unit = cfg.funcs.foreach(apply)

  private def apply(func: Func): Unit = {

    val result = new DumpStepToNodeId(func)
    result.compute

    for {
      (step, posSet) <- result.abruptMap
      sorted = posSet.toSeq.sorted(Ordering.by[(Pos, Int), Pos](_._1))
      ((pos, nodeId), idx) <- sorted.zipWithIndex
    } do result.stepToNodeId.safeAdd(s"${step}|?${idx + 1}", nodeId)

    dumpJson(
      name = s"stepToNodeId for ${func.name}",
      data = result.stepToNodeId,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/stepIdToNodeId/${func.id}.json",
      silent = true,
    )
  }
}

extension [K, V](m: MMap[K, MSet[V]])
  def safeAdd(key: K, value: V): MSet[V] =
    m.getOrElseUpdate(key, MSet.empty) += value
