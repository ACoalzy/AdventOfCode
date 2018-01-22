import Days._

import scala.io.Source

object DayRunner extends App {
  println(Day15.countXGenerationMatches(Day15.Generator(634, 16807, 4), Day15.Generator(301, 48271, 8), 5000000))
}
