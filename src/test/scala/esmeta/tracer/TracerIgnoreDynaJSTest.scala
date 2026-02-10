package esmeta.tracer

import esmeta.{TEST_DIR, LOG_DIR}
import esmeta.util.{ConcurrentPolicy as CP}
import java.nio.file.Paths

class TracerIgnoreDynaJSTest extends TracerTest {
  val name: String = "tracerIgnoreDynaJSTest"

  given WorkDir = WorkDir(s"$LOG_DIR/tracer-ignore-dynajs")

  // registration
  def init: Unit =
    for (files <- Paths.get(TEST_DIR).resolve("tracer").toFile.listFiles) do {
      // val thisTarget = TracerTest.targetTests.filter(t => t)
      val testName = files.getName.stripSuffix(".js")
      val test = TracerTest.targetTests
        .find(t => t.absPath == files.getAbsolutePath())
        .get
      check(testName) {
        TraceDumper(
          targetTests = List(test),
          removed = Nil,
          isTest262 = false,
          targetTracer = Set(TargetKind.DynaJS),
          handleNotSupportedAsFail = true,
          useStdout = false,
        )(using TracerTest.test262).test

        val summary = TraceComparer(
          TracerTest.test262.cfg,
          TraceRestore.restore(List(test.absPath)),
          targetTracer = Set(TargetKind.DynaJS),
          useStdout = false,
        ).test
        val f = summary.failCount
        if (f > 0) fail(s"$f tests are failed.")
      }
    }

  init
}
