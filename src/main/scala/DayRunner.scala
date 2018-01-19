import Days._

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day8Instructions.txt").getLines()

  val instructions = Day8.buildInstructions(lines.toList)
  println(Day8.runInstructions(instructions).maxBy(_._2))
  println(Day8.runInstructionsFullHistory(instructions).flatten.maxBy(_._2))
}
