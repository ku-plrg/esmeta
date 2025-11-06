package esmeta.tracer

import esmeta.*

case class WorkDir(path: String)

/* workdir for tracers, can't use /tmp/... cause of docker volume */
val TRACER_WORKDIR = WorkDir(s"$LOG_DIR/tempfiles-esmetajalangi")
