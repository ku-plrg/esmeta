package esmeta.tracer

import esmeta.{LOG_DIR, TEST_DIR}
import esmeta.util.{ConcurrentPolicy as CP}
import java.nio.file.Paths

class TracerIgnoreFullTest extends TracerTest {
  val name: String = "tracerIgnoreFullTest"

  given WorkDir = WorkDir(s"$LOG_DIR/tracer-ignore-full")

  // registration
  def init: Unit =

    // TODO cache collision...
    ???
    TraceDumper(
      targetTests = TracerTest.targetTests,
      removed = Nil,
      isTest262 = false,
      nodeprof = true,
      useStdout = false,
    )(using TracerTest.test262).test

    for (files <- Paths.get(TEST_DIR).resolve("tracer").toFile.listFiles) do {
      val testName = files.getName.stripSuffix(".js")
      check(testName) {
        val summary = TraceComparer(
          TracerTest.test262.cfg,
          TraceRestore.restore(TracerTest.targetTests.map(t => t.absPath)),
          useStdout = false,
        ).test
        val f = summary.failCount
        if (f > 0) fail(s"$f tests are failed.")
      }
    }

  init
}
