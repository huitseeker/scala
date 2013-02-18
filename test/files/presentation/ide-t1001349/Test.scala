import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {

  override def execute(): Unit = {
    // make sure typer is done .. if there was no NPE, this is ok
    // the exact computed type does not matter, in fact it's best
    // if imports are missing
    try {
      sourceFiles foreach (src => askLoadedTyped(src).get)
      reporter.println("NPE 1001349 OK")
    }
    catch {
      case e : NullPointerException => reporter.println("NPE 1001349 FAILED")
    }
  }
}
