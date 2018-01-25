package Days

object Day20 {

  case class Point(x: Int, y: Int, z: Int) {
    def manhattan: Int = math.abs(x) + math.abs(y) + math.abs(z)
    def +(p: Point): Point = Point(x + p.x, y + p.y, z + p.z)
  }

  case class Particle(p: Point, v: Point, a: Point) {
    private def compare(x: Point, y: Point): Boolean = x.manhattan == y.manhattan

    def isSlower(part: Particle): Boolean = {
      if (compare(a, part.a) && compare(v, part.v)) p.manhattan < part.p.manhattan
      else if (compare(a, part.a)) v.manhattan < part.v.manhattan
      else a.manhattan < part.a.manhattan
    }

    def move: Particle = Particle(p + v + a, v + a, a)
  }

  val maxPoint = Point(Int.MaxValue, Int.MaxValue, Int.MaxValue)
  val maxParticle = Particle(maxPoint, maxPoint, maxPoint)

  def parsePoint(s: String): Point = {
    val is = s.split(',')
    Point(is(0).toInt, is(1).toInt, is(2).toInt)
  }

  def parseParticle(s: String): Particle = {
    val acceptableChars = List('-', ',')
    val points = s.split(", ").map(s => s.filter(c => c.isDigit || acceptableChars.contains(c)))
    Particle(parsePoint(points(0)), parsePoint(points(1)), parsePoint(points(2)))
  }

  def findSlowestParticle(lines: List[String]): Int = {
    val particles = lines.map(parseParticle(_))
    val slowest = particles.fold(maxParticle)((p1, p2) => if (p1.isSlower(p2)) p1 else p2)
    particles.indexOf(slowest)
  }

  private def collides(p: Particle, ps: List[Particle]): Boolean = {
    ps.filter(p2 => p.p == p2.p).size > 1
  }

  private def simulateX(particles: List[Particle], x: Int): (List[Particle], Int) = {
    val remaining = 1.to(x).foldLeft(particles)((ps, _) => {
      val newPs = ps.map(p => p.move)
      newPs.filter(p => !collides(p, newPs))
    })

    (remaining, particles.size - remaining.size)
  }

  /**
    * Brute force method which ends after 100 iterations with no collisions
    *
    * @param lines
    * @return
    */
  def countCollisions(lines: List[String]): Int = {
    def go(acc: Int, particles: List[Particle]): Int = {
      val simulation = simulateX(particles, 100)
      if (simulation._2 == 0) acc
      else go(acc + simulation._2, simulation._1)
    }

    val particles = lines.map(parseParticle(_))
    go(0, particles)
  }

}
