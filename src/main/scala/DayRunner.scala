import Days.Day10.State
import Days._

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day10Lengths.txt").map(_.toInt)
  println(Day10.denseHash(lines.toList, State(0 to 255, 0, 0)))
}
