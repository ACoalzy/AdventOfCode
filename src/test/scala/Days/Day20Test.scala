package Days

import Days.Day20.{Particle, Point}
import org.scalatest.FunSuite

class Day20Test extends FunSuite {

  test("parse particle") {
    assert(Day20.parseParticle("p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>") == Particle(Point(3, 0, 0), Point(2, 0, 0), Point(-1, 0, 0)))
  }

  test("find slowest particle with example input") {
    assert(Day20.findSlowestParticle(List("p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>", "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>")) == 0)
  }

  test("count collions with example input") {
    assert(Day20.countCollisions(List("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0> ", "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0> ", "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0> ", "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")) == 3)
  }

}
