import Days.Day2

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day2CheckSum.txt").getLines()

  println(Day2.checksumDivision(lines.toSeq))
}
