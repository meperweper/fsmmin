
object Main {

  case class State(n: String)

  case class Event(e: String)

  case class FSM(V: Seq[Event], Q: Seq[State], q: State, F: Seq[State], t: Seq[Edge])

  object ZeroState extends State("--ZeroState--")

  case class Edge(cur: State, e: Event, next: State)


  def bfs(queue: Seq[State], edges: Seq[Edge], attended: Seq[State]): Seq[State] = {
    if (queue.isEmpty)
      attended
    else {
      val state = queue.head
      val next = edges.filter {
        case Edge(`state`, _, end) if end != `state` && !attended.contains(end) => true
        case _ => false
      }.map(_.next).distinct
      bfs(queue.tail ++ next, edges, attended ++ next)
    }
  }

  def curried(edges: Seq[Edge]) = {
    (cur: State, e: Event) =>
      val buf = edges.collect {
        case Edge(`cur`, `e`, next) => next
      }
      (cur, buf) match {
        case (ZeroState, _) => Seq()
        case (_, buff) if buff.isEmpty => Seq(ZeroState)
        case _ => buf
      }
  }

  def buildTable(events: Seq[Event], n: Int, finalSt: Seq[State], function: (State, Event) => Seq[State], states: Seq[State]) = {
    import scala.collection._
    var queue = mutable.Queue.empty[(State, State)]
    val marked = mutable.Map.empty[(State, State), Boolean]
    for {
      i <- states
      j <- states
    } {
      marked += ((i, j) -> false)
    }
    for {
      i <- states
      j <- states
    } {
      if (!marked.getOrElse((i, j), false)
        && (finalSt.contains(i) != finalSt.contains(j))) {
        marked += ((i, j) -> true) += ((j, i) -> true)
        queue += ((i, j))
      }
    }
    while (queue.nonEmpty) {
      val (u, v) = queue.head
      queue = queue.tail
      for {
        c <- events
      } {
        for {
          r <- function(u, c)
          s <- function(v, c)
        } {
          if (!marked.getOrElse((r, s), false)) {
            println(r + "," + s)
            marked += ((r, s) -> true) += ((s, r) -> true)
            queue += ((r, s))
          }
        }
      }
    }
    marked += ((ZeroState, ZeroState) -> false) //hack
  }

  def parse[S](s: String, producer: (String) => S): Seq[S] = {
    val content = s.substring(s.indexOf('{') + 1, s.indexOf('}'))
    content.replace(" ", "").trim.split(",").map(producer)
  }


  def parseVovaFsm(fsmstr: String) = {
    val file = fsmstr.lines.toVector
    val edges = (for (x <- 1 to file.head.toInt) yield {
      file(x).split("(?<!\\G\\w+)\\s").map {
        end =>
          val Array(endd, char) = end.split(" ")
          Edge(State(x.toString), Event(char), State(endd))
      }
    }).flatten
    val start: State = State(file(file.head.toInt))
    val ends: Array[State] = file(file.head.toInt + 1).split(" ").filter(_.trim == "").map(State)
    val other = ends.foldLeft(edges.flatMap(x => Vector(x.cur, x.next)).distinct.toSet - start) { case (set, cur) => set - cur }.toSeq
    FSM(edges.map(_.e), other, start, ends, edges)
  }

  def parseFSM(fsmstr: String) = {
    val file = fsmstr.lines.toVector
    def parser[S](n: String, p: (String) => S) = parse(file.find(s => s.startsWith(n)).get, p)
    val V = parser("V", Event.apply)
    val Q = parser("Q", State.apply)
    val Seq(q) = parser("q", State.apply)
    val F = parser("F", State.apply)
    val sliced = file.slice(file.zipWithIndex.find(_._1.startsWith("T=")).get._2 + 1, file.size)
    val T = sliced.map {
      edge =>
        val Array(left, right) = edge.split("=>")
        val rst = State(right.trim)
        val Array(leftst, event) = left.replace("(", "").replace(")", "").replace(" ", "").split(",")
        Edge(State(leftst), Event(event), rst)
    }.toIndexedSeq
    FSM(V, Q, q, F, T)
  }


  def minimize(fsm: FSM) = {
    val edges: Seq[Edge] = fsm.t
    val Q: Seq[State] = fsm.Q :+ ZeroState
    val states = fsm.F ++ Q :+ fsm.q
    val rEdge = edges.map(st => Edge(st.next, st.e, st.cur))
    val table = buildTable(fsm.V, states.size, fsm.F, curried(rEdge), states)
    val equals = table.filter(z => !z._2 && z._1._1 != z._1._2)
    val equalMap: Map[State, State] = equals.foldLeft(Map.empty[(State, State), Boolean]) {
      (map, cur) =>
        if (map.contains(cur._1) || map.contains(cur._1.swap))
          map
        else
          map + cur
    }.foldLeft(Set.empty[Seq[State]]) {
      (seq: Set[Seq[State]], cur) =>
        (seq.find(x => x.contains(cur._1._1))
          , seq.find(x => x.contains(cur._1._2))) match {
          case (None, None) => seq + Seq(cur._1._1, cur._1._2)
          case (Some(x), None) => (seq - x) + (x :+ cur._1._2)
          case (None, Some(x)) => (seq - x) + (x :+ cur._1._1)
          case _ => seq
        }
    }.toSeq.flatMap {
      x =>
        val newSt = State(x.map(_.n).mkString("$"))
        x.map((_, newSt))
    }.toMap
    val newEdges = edges.map {
      x =>
        Edge(
          equalMap.getOrElse(x.cur, x.cur),
          x.e,
          equalMap.getOrElse(x.next, x.next)
        )
    }.distinct
    val newStates: Seq[State] = newEdges.flatMap(x => Seq(x.cur, x.next)).distinct
    val fs: Seq[State] = fsm.F.flatMap(x => equalMap.get(x).map(Seq(_)).getOrElse(Seq())).distinct
    val q = equalMap.getOrElse(fsm.q, fsm.q)
    FSM(fsm.V, (newStates.toSet -- fs.toSet - q).toSeq, q, fs, newEdges)
  }


  def writeFSM(fsm: FSM) = {
    s"V={${fsm.V.map(_.e).mkString(",")}}\n" +
      s"F={${fsm.F.map(_.n).mkString(",")}}\n" +
      s"q={${fsm.q.n}}\n" +
      s"Q={${fsm.Q.map(_.n).mkString(",")}}\n" +
      s"T=\n" +
      fsm.t.map(x => s"(${x.cur.n},${x.e.e})=>${x.next.n}").mkString("\n")
  }

  def main(args: Array[String]) = {
    val fsm = parseFSM(io.Source.fromFile("D:\\dev\\src\\fsmmin\\src\\test\\resources\\example2.fsm").mkString)
    val minfsm = minimize(fsm)
    val strfsm = writeFSM(minfsm)
    println(strfsm)
  }
}
