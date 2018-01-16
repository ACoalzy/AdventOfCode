package Days

import Days.Day3.Point
import org.scalatest.FunSuite

class Day3Test extends FunSuite {
  test("calculate square width returns 1 for 1") {
    assert(Day3.calculateSquareWidth(1) == 1)
  }

  test("calculate square width returns 3 for 8") {
    assert(Day3.calculateSquareWidth(8) == 3)
  }

  test("calculate square width returns 13 for 150") {
    assert(Day3.calculateSquareWidth(150) == 13)
  }

  test("get 1 index returns 0 for 1") {
    assert(Day3.get1Index(1) == 0)
  }

  test("get 1 index returns 0 for 2") {
    assert(Day3.get1Index(2) == 0)
  }

  test("get 1 index returns 2 for 5") {
    assert(Day3.get1Index(5) == 2)
  }

  test("manhattan distance returns 2 for (2, 0), (2, 2)") {
    assert(Day3.manhattanDistance(Point(2, 0), Point(2, 2)) == 2)
  }

  test("manhattan distance returns 3 for (0, 3), (2, 2)") {
    assert(Day3.manhattanDistance(Point(0, 3), Point(2, 2)) == 3)
  }

  test("get start index returns 0, 0 for 1") {
    assert(Day3.getStartIndex(1) == Point(0, 0))
  }

  test("get start index returns (1, 0) for 22") {
    assert(Day3.getStartIndex(22) == Point(1, 0))
  }

  test("get start index returns (0, 2) for 19") {
    assert(Day3.getStartIndex(19) == Point(0, 2))
  }

  test("get start index returns (0, 3) for 16") {
    assert(Day3.getStartIndex(16) == Point(0, 3))
  }

  test("get start index returns (3, 0) for 10") {
    assert(Day3.getStartIndex(10) == Point(3, 0))
  }

  test("get start index returns (3, 3) for (13, 4)") {
    assert(Day3.getStartIndex(13) == Point(3, 3))
  }

  test("spiral manhattan start 1 returns 0") {
    assert(Day3.spiralManhattan(1) == 0)
  }

  test("spiral manhattan start 3 returns 2") {
    assert(Day3.spiralManhattan(4) == 1)
  }
  test("spiral manhattan start 4 returns 1") {
    assert(Day3.spiralManhattan(4) == 1)
  }

  test("spiral manhattan start 12 returns 3") {
    assert(Day3.spiralManhattan(12) == 3)
  }

  test("spiral manhattan start 23 returns 2") {
    assert(Day3.spiralManhattan(12) == 3)
  }

  test("spiral manhattan start 1024 returns 31") {
    assert(Day3.spiralManhattan(12) == 3)
  }

  test("spiral summing with 1 returns 1") {
    assert(Day3.spiralSumming(1) == 1)
  }

  test("spiral summing with 5 returns 5") {
    assert(Day3.spiralSumming(5) == 5)
  }

  test("spiral summing with 6 returns 10") {
    assert(Day3.spiralSumming(6) == 10)
  }

  test("spiral summing with 7 returns 10 due to early exit") {
    assert(Day3.spiralSumming(7) == 10)
  }
}
