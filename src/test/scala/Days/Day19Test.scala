package Days

import Days.Day19.Point
import org.scalatest.FunSuite

class Day19Test extends FunSuite {

  test("find start spots first |") {
    assert(Day19.findStart("     |    |") == Point(0, 5))
  }

  test("is path returns true for non ' ' ") {
    assert(Day19.isPath(Point(0, 2), Seq("  A  ")))
  }

  test("is path returns false for ' ' ") {
    assert(Day19.isPath(Point(0, 2), Seq("   A  ")) == false)
  }

  test("is path returns false for out of bounds") {
    assert(Day19.isPath(Point(1, 2), Seq("  A  ")) == false)
  }

  test("follow path with example input") {
    val path = Seq("     |          ", "     |  +--+    ", "     A  |  C    ", " F---|----E|--+ ", "     |  |  |  D ", "     +B-+  +--+ ")
    assert(Day19.followPath(path) == "||A||+B-+|-|+--+C||+--+D+--|E----|---F")
  }

}
