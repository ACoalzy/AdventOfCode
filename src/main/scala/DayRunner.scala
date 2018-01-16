import Days.{Day3, Day4}

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day4Passphrases.txt").getLines()

  println(Day4.countNoAnagramPassphrases(lines.toSeq))
}
