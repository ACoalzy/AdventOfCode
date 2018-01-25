import Days._

import scala.io.Source

object DayRunner extends App {
  val particles = Source.fromResource("Days/Day20Particles.txt").getLines().toList
  println(Day20.findSlowestParticle(particles))
  println(particles.size - Day20.countCollisions(particles))
}
