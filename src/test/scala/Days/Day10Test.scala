package Days

import Days.Day10.State
import org.scalatest.FunSuite

class Day10Test extends FunSuite {

  test("hash 0, 1, 2, 3, 4 with 3, 4, 1, 5") {
    assert(Day10.hash(List(3, 4, 1, 5), State(0 to 4, 0, 0)).list == Seq(3, 4, 2, 1, 0))
  }

  test("dense hash empty list") {
    assert(Day10.denseHash(List(), State(0 to 255, 0, 0)) == "a2582a3a0e66e6e86e3812dcb672a272")
  }

  test("dense hash AoC 2017") {
    assert(Day10.denseHash("AoC 2017".map(_.toInt).toList, State(0 to 255, 0, 0)) == "33efeb34ea91902bb2f59c9920caa6cd")
  }

  test("dense hash 1,2,3") {
    assert(Day10.denseHash("1,2,3".map(_.toInt).toList, State(0 to 255, 0, 0)) == "3efbe78a8d82f29979031a4aa0b16a9d")
  }

  test("dense hash 1,2,4") {
    assert(Day10.denseHash("1,2,4".map(_.toInt).toList, State(0 to 255, 0, 0)) == "63960835bcdc130f0b66d7ff4f6a5a8e")
  }

}
