import scala.tools.nsc.interactive.tests._
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.Response

object Test extends InteractiveTest {
  override def execute(): Unit = loadedAskForResponse()

  private def loadSourceAndWaitUntilTypechecked(sf: SourceFile): Unit = {
    compiler.askToDoFirst(sf)
    val res = new Response[Unit]
    compiler.askReload(List(sf), res)
    res.get
    askLoadedTyped(sf).get
  }

  def loadedAskForResponse() {
    val sf = sourceFiles.find(_.file.name == "A.scala").head
    loadSourceAndWaitUntilTypechecked(sf)
    val responses = new Array[Response[Unit]](10024)
    var i=0; while (i<10024) {
      if (i % 100 == 0) println("askForResponse " + i)
      responses(i) = compiler.askForResponse{ () => Thread.sleep(1) }
      i += 1
    }
    i=0; while (i<1024) {
      responses(i).get
      i += 1
    }

    println("done")
  }
}
