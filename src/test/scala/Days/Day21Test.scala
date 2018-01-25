package Days

import Days.Day21.Pattern
import org.scalatest.FunSuite

class Day21Test extends FunSuite {

  test("parse pattern") {
    assert(Day21.parsePattern("../.#") == Pattern(List("..", ".#")))
  }

  test("rotations gets all 2x2 rotations") {
    val rotations = Set(Pattern(List(".#", "..")), Pattern(List("#.", "..")), Pattern(List("..", "#.")), Pattern(List("..", ".#")))
    assert(Day21.rotations(List("..", ".#")) == rotations)
  }

  test("find alternatives returns all 8 alts") {
    val p = Pattern(List(".#.", "..#", "###"))
    val alternatives = Set(Pattern(List(".##", "#.#", "..#")), Pattern(List(".#.", "..#", "###")), Pattern(List("#..", "#.#", "##.")), Pattern(List("##.", "#.#", "#..")), Pattern(List("###", "#..", ".#.")), Pattern(List("..#", "#.#", ".##")), Pattern(List(".#.", "#..", "###")), Pattern(List("###", "..#", ".#.")))
    assert(p.findMatches == alternatives)
  }

  test("split returns same size 2 pattern") {
    val p = Pattern(List("..", "##"))
    assert(p.split == List(List(p)))
  }

  test("split returns same size 3 pattern") {
    val p = Pattern(List("...", "###", ".#."))
    assert(p.split == List(List(p)))
  }

  test("split returns 4 size 2 patterns") {
    val p = Pattern(List("....", "...#", "..##", ".###"))
    assert(p.split == List(List(Pattern(List("..", "..")), Pattern(List("..", ".#"))), List(Pattern(List("..", ".#")), Pattern(List("##", "##")))))
  }

  test("combine combins 4 size 2 patterns") {
    val p = Pattern(List("....", "...#", "..##", ".###"))
    assert(Day21.combine(List(List(Pattern(List("..", "..")), Pattern(List("..", ".#"))), List(Pattern(List("..", ".#")), Pattern(List("##", "##"))))) == p)
  }

  test("enhanceXTimes with example input") {
    val rules = List("../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#")
    assert(Day21.enhanceXTimes(rules, 2) == Pattern(List("##.##.", "#..#..", "......", "##.##.", "#..#..", "......")))
  }

  test("count on pixels with example result") {
    val p = Pattern(List("##.##.", "#..#..", "......", "##.##.", "#..#..", "......"))
    assert(Day21.countOnPixels(p) == 12)
  }

}
