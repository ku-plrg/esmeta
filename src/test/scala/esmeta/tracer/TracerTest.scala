package esmeta.tracer

import esmeta.*
import esmeta.spec.Spec
import esmeta.test262.*
import esmeta.tracer.*
import esmeta.tracer.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.nio.file.*

trait TracerTest extends ESMetaTest {
  def category: String = "tracer"
}
object TracerTest {
  lazy val test262 = Test262(Test262.currentVersion, ESMetaTest.cfg)

  lazy val testFiles = Paths.get(TEST_DIR).resolve("tracer").toFile.listFiles

  lazy val ((targetTests, removed), isTest262) = TraceDumper.computeTarget(
    Some(testFiles.map(f => f.getAbsolutePath()).toList),
  )(using test262)

}
