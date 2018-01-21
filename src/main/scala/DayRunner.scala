import Days._

import scala.io.Source

object DayRunner extends App {
  val steps = Source.fromResource("Days/Day13Scan.txt").getLines()
  val layers = Day13.buildLayers(steps.toList)
  println(Day13.tripSeverity(layers, 0))
  println(Day13.minDelay(layers))
}
