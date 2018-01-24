import Days._

import scala.io.Source

object DayRunner extends App {
  val path = Source.fromResource("Days/Day19RoutingPath.txt").getLines().toSeq
  val bannedChars = List('|', '-', '+')
  println(Day19.followPath(path).filter(c => !bannedChars.contains(c)))
  println(Day19.followPath(path).size)
}
