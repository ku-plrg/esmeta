package esmeta.tracer
package util

import esmeta.util.Appender.Rule

class Stringifier {
  private def traceWithLoc(trace: Trace): String =
    trace.loc match
      case Some(loc) =>
        s"${trace.toString()} (${loc.start.line}-${loc.end.line})"
      case None => trace.toString()

  given elemRule: Rule[TracerElem] = (app, elem) => {
    elem match
      case tree: TraceTree              => app >> tree
      case diffTree: TraceDiff.DiffTree => app >> diffTree
  }

  given treeRule: Rule[TraceTree] = (app, tree) => {
    def prettyString(roots: Seq[TraceTree.Node]): String = {
      val sb = new StringBuilder

      def render(nodes: Seq[TraceTree.Node], prefix: String): Unit = {
        nodes.zipWithIndex.foreach {
          case (node, idx) =>
            val isLast = idx == nodes.size - 1
            val connector = if (isLast) "`- " else "|- "
            sb.append(prefix)
              .append(connector)
              .append(traceWithLoc(node.open))
              .append("\n")
            val nextPrefix = prefix + (if (isLast) "   " else "|  ")
            render(node.children, nextPrefix)
            node.close.foreach { t =>
              sb.append(prefix)
                .append(connector)
                .append(" ")
                .append(traceWithLoc(t))
                .append("\n")
            }
        }
      }

      render(roots, "")
      sb.result()
    }
    app >> prettyString(tree.roots)
  }

  given diffTreeRule: Rule[TraceDiff.DiffTree] = (app, diffTree) => {
    def tagString(tag: TraceDiff.DiffTag): String = tag match
      case TraceDiff.DiffTag.Ignored(reason, side) =>
        if (reason.nonEmpty) s"Ignored($side, $reason)" else s"Ignored($side)"
      case other => other.toString

    def prettyString(roots: Vector[TraceDiff.DiffNode]): String = {
      val sb = new StringBuilder
      val red = "\u001b[31m"
      val green = "\u001b[32m"
      val yellow = "\u001b[33m"
      val blue = "\u001b[34m"
      val gray = "\u001b[90m"
      val reset = "\u001b[0m"

      def render(
        nodes: Seq[TraceDiff.DiffNode],
        prefix: String,
      ): Unit = {
        nodes.zipWithIndex.foreach {
          case (node, idx) =>
            val isLast = idx == nodes.size - 1
            val connector = if (isLast) "`- " else "|- "
            val color = node.tag match
              case TraceDiff.DiffTag.LeftOnly      => blue
              case TraceDiff.DiffTag.RightOnly     => yellow
              case TraceDiff.DiffTag.Ignored(_, _) => gray
              case TraceDiff.DiffTag.Different     => red
              case _                               => ""
            val colorReset = if (color.nonEmpty) reset else ""
            sb.append(color)
              .append(prefix)
              .append(connector)
              .append(s"[${tagString(node.tag)}] ")
            val leftOpen = node.left.map(node => traceWithLoc(node.open))
            val rightOpen = node.right.map(node => traceWithLoc(node.open))
            val openStr = (leftOpen, rightOpen) match
              case (Some(l), Some(r)) if l != r =>
                s"L: $l | R: $r"
              case (Some(l), Some(_)) =>
                l
              case (Some(l), None) =>
                s"L: $l"
              case (None, Some(r)) =>
                s"R: $r"
              case _ =>
                ""
            sb.append(openStr)
            sb.append(colorReset)
            sb.append("\n")
            val nextPrefix = prefix + (if (isLast) "   " else "|  ")
            render(node.children, nextPrefix)
            val leftClose = node.left.flatMap(_.close).map(traceWithLoc)
            val rightClose = node.right.flatMap(_.close).map(traceWithLoc)
            val closeStr = (leftClose, rightClose) match
              case (Some(l), Some(r)) if l != r =>
                Some(s"L: $l | R: $r")
              case (Some(l), Some(_)) =>
                Some(l)
              case (Some(l), None) =>
                Some(s"L: $l")
              case (None, Some(r)) =>
                Some(s"R: $r")
              case _ =>
                None
            closeStr.foreach { close =>
              sb.append(color)
                .append(prefix)
                .append(connector)
                .append(" ")
                .append(close)
                .append(colorReset)
                .append("\n")
            }
        }
      }

      render(roots, "")
      sb.result()
    }

    app >> prettyString(diffTree.roots)
  }

}
