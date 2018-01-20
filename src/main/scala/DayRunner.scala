import Days._

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day9Stream.txt")

  // println(Day9.scoreStream(lines))
  println(Day9.countGarbage(lines))
}
