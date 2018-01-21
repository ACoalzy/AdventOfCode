package Days

import org.scalatest.FunSuite

class Day13Test extends FunSuite {

  test("trip severity with all layers size 1") {
    assert(Day13.tripSeverity(Map(0 -> 1, 1 -> 1, 2 -> 1), 0) == Some(3))
  }

  test("trip severity with example input") {
    assert(Day13.tripSeverity(Map(0 -> 3, 1 -> 2, 4 -> 4, 6 -> 4), 0) == Some(24))
  }

  test("min delay with example input") {
    assert(Day13.minDelay(Map(0 -> 3, 1 -> 2, 4 -> 4, 6 -> 4)) == 10)
  }

}
