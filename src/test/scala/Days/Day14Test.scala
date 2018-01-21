package Days

import Days.Day14.Coord
import org.scalatest.FunSuite

class Day14Test extends FunSuite {

  test("count used bits with example input") {
    assert(Day14.countUsedBits("flqrgnkx") == 8108)
  }

  test("count regions with example input") {
    assert(Day14.countRegions("flqrgnkx") == 1242)
  }

  test("fillgroup handles lone squares") {
    assert(Day14.fillGroup(Coord(1, 1), Seq("000", "010", "000")) == Set(Coord(1, 1)))
  }

  test("fillgroup handles corner shapes") {
    assert(Day14.fillGroup(Coord(0, 0), Seq("11", "01")) == Set(Coord(0, 0), Coord(0, 1), Coord(1, 1)))
  }

  test("fillgroup handles U shapes") {
    assert(Day14.fillGroup(Coord(0, 0), Seq("101", "101", "111")) == Set(Coord(0, 0), Coord(0, 2), Coord(1, 0), Coord(1, 2), Coord(2,0), Coord(2,1), Coord(2,2)))
  }

}
