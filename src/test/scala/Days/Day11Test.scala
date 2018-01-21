package Days

import org.scalatest.FunSuite

class Day11Test extends FunSuite {

  test("shortestPathLength 3 same direction returns 3") {
    assert(Day11.shortestPathLength(List("ne", "ne", "ne")) == 3)
  }

  test("shortestPathLength 2 out 2 back returns 0") {
    assert(Day11.shortestPathLength(List("ne", "ne", "sw", "sw")) == 0)
  }

  test("shortestPathLength circle returns 0") {
    assert(Day11.shortestPathLength(List("n", "n", "sw", "sw", "se", "se")) == 0)
  }

  test("shortestPathLength offset directions are halved") {
    assert(Day11.shortestPathLength(List("ne", "ne", "s", "s")) == 2)
  }

  test("shortestPathLength offset directions are halved and maintains others") {
    assert(Day11.shortestPathLength(List("se", "sw", "se", "sw", "se")) == 3)
  }

  test("getFurthestEverAway handles end") {
    assert(Day11.getFurthestEverAway(List("n", "n", "n")) == 3)
  }

  test("getFurthestEverAway handles halfway") {
    assert(Day11.getFurthestEverAway(List("n", "n", "s", "s")) == 2)
  }

  test("getFurthestEverAway handles multiple same distance") {
    assert(Day11.getFurthestEverAway(List("n", "n", "nw", "sw", "s", "se")) == 3)
  }

}
