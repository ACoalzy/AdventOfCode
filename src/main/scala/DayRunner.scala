import Days._

import scala.io.Source

object DayRunner extends App {
  val particles = Source.fromResource("Days/Day21EnhancementRules.txt").getLines().toList
  println(Day21.countOnPixels(Day21.enhanceXTimes(particles, 18)))
}
