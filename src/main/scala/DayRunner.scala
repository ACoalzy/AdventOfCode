import Days.Day10.State
import Days._

import scala.io.Source

object DayRunner extends App {
  val steps = Source.fromResource("Days/Day11Route.txt").getLines().next().split(",").toList
  println(Day11.shortestPathLength(steps))
  println(Day11.getFurthestEverAway(steps))
}
