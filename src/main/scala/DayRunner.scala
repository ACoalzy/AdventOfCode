import Days._

import scala.io.Source

object DayRunner extends App {
  val instructions = Source.fromResource("Days/Day23Instructions.txt").getLines().toList
  println(Day23.countNonPrimesBetween(108400, 125400, 17))
}
