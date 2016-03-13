import Main.{Edge, Event, State}
import org.scalatest._

class MainSpec extends FlatSpec with Matchers {

  "Minimizer" should "minimize fine" in {
    val strfsm = io.Source.fromURL(getClass.getResource("/example2.fsm")).mkString
    val foo =  Main.minimize(Main.parseFSM(strfsm))
    val bar = Vector(Edge(State("b$a"),Event("1"),State("b$a")),
      Edge(State("b$a"),Event("0"),State("h")),
      Edge(State("h"),Event("1"),State("d$c")),
      Edge(State("h"),Event("0"),State("d$c")),
      Edge(State("d$c"),Event("0"),State("e")),
      Edge(State("d$c"),Event("1"),State("f$g")),
      Edge(State("e"),Event("1"),State("f$g")),
      Edge(State("e"),Event("0"),State("f$g")),
      Edge(State("f$g"),Event("1"),State("f$g")),
      Edge(State("f$g"),Event("0"),State("f$g")))
    assert( foo == bar)
  }
  "Minimizer" should "minimize fine here too" in {
    val strfsm = io.Source.fromURL(getClass.getResource("/example2.fsm")).mkString

    val foo =  Main.minimize(Main.parseFSM(strfsm))
    val bar = Vector(
      Edge(State("b$a"),Event("1"),State("b$a")),
      Edge(State("b$a"),Event("0"),State("h")),
      Edge(State("h"),Event("1"),State("d$c")),
      Edge(State("h"),Event("0"),State("d$c")),
      Edge(State("d$c"),Event("0"),State("e")),
      Edge(State("d$c"),Event("1"),State("f$g")),
      Edge(State("e"),Event("1"),State("f$g")),
      Edge(State("e"),Event("0"),State("f$g")),
      Edge(State("f$g"),Event("1"),State("f$g")),
      Edge(State("f$g"),Event("0"),State("f$g")))
    assert(foo == bar)
  }

}