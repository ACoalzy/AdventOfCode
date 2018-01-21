import Days.Day10.State
import Days._

import scala.io.Source

object DayRunner extends App {
  val steps = Source.fromResource("Days/Day12Pipes.txt").getLines()
  val pipes = Day12.buildPipes(steps.toList)
  println(Day12.countPipesInGroup(pipes, "0"))
  println(Day12.countPipeGroups(pipes))
}
