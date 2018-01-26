import Days._

import scala.io.Source

object DayRunner extends App {
  val nodes = Source.fromResource("Days/Day22InfectedNodes.txt").getLines().toVector
  println(Day22.countInfections(nodes, 10000))
}
