package esmeta.tracer

import esmeta.*
import esmeta.util.BaseUtils.raise
import scala.collection.mutable.ArrayBuffer
class Analysis {

  val ts = ArrayBuffer.empty[Trace]

  def append(tk: Trace): Unit = this.synchronized { ts.append(tk) }
  def __print(str: String): Unit = this.synchronized {
    ts.append(Trace.Print(str))
  }

}
