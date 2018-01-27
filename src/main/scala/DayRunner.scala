import Days._

import scala.io.Source

object DayRunner extends App {
  val components = Source.fromResource("Days/Day24Components.txt").getLines().toVector
  println(Day24.longestStrongestBridge(components).strength)
}
