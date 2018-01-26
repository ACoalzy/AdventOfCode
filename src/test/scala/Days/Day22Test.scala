package Days

import Days.Day22.{Carrier, Point, Map}
import org.scalatest.FunSuite

class Day22Test extends FunSuite {

  test("test carrier rotation left") {
    assert(Day22.rotate(Point(-1, 0), Day22.Left) == Point(0, -1))
  }

  test("test carrier rotation right") {
    assert(Day22.rotate(Point(1, 0), Day22.Right) == Point(0, -1))
  }

  test("extend wraps map in outer layer") {
    val map = Map.create(Vector("..#", "#..", "..."))
    val car = Carrier(Point(1,1), Point(1,1))
    assert(Day22.extend(map, car) == (Map.create(Vector(".....", "...#.", ".#...", ".....", ".....")), Carrier(Point(2,2), Point(1,1))))
  }

  test("count infections with example input") {
    val map = Vector("..#", "#..", "...")
    assert(Day22.countInfections(map, 70) == 41)
  }

  test("count infections with long example input") {
    val map = Vector("..#", "#..", "...")
    assert(Day22.countInfections(map, 10000) == 5587)
  }

}
