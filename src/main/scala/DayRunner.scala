import Days._

import scala.io.Source

object DayRunner extends App {
  val ins = Source.fromResource("Days/Day18Instructions.txt").getLines()
  println(Day18.runPair(ins.toSeq))
}
