import Days.{Day3, Day4, Day5}

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day5JumpInstructions.txt").getLines()

  println(Day5.countStrangeJumpsToExit(lines.toVector))
}
