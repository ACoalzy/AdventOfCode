package Days

import org.scalatest.FunSuite

class Day17Test extends FunSuite {

  test("insert step handles 0 -> 1 with step of 3") {
    assert(Day17.insertStep(1, List(0), 0, 3)._1 == List(0, 1))
  }

  test("insert step handles 1 -> 2 with step of 3") {
    assert(Day17.insertStep(2, List(0, 1), 1, 3)._1 == List(0, 2, 1))
  }

  test("insert step handles 8 -> 9 with step of 3") {
    assert(Day17.insertStep(9, List(0, 5, 7, 2, 4, 3, 8, 6, 1), 6, 3)._1 == List(0, 9, 5, 7, 2, 4, 3, 8, 6, 1))
  }

  test("spinlock handles example input") {
    assert(Day17.spinlock(3) == 638)
  }

}
