import Days.{Day3, Day4, Day5, Day6}

import scala.io.Source

object DayRunner extends App {
  val line = Source.fromResource("Days/Day6Blocks.txt").getLines().next()

  println(Day6.countRedistributionCycleLoopSize(line.split("\\s+").map(_.toInt)))
}
