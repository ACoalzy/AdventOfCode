import Days._

import scala.io.Source

object DayRunner extends App {
  val moves = Source.fromResource("Days/Day16DanceMoves.txt").getLines().next()
  println(Day16.justOneDance(moves.split(",").toList, 16))
  println(Day16.xDances(moves.split(",").toList, 16, 1000000000L))
}
