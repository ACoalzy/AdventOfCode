package Days

import org.scalatest.FunSuite

class Day9Test extends FunSuite {

  test("scoreStream one empty group") {
    val chars = "{}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 1)
  }

  test("scoreStream 3 nested groups") {
    val chars = "{{{}}}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 6)
  }

  test("scoreStream 1 group with 2 children") {
    val chars = "{{}{}}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 5)
  }

  test("scoreStream 6 groups mixed nesting") {
    val chars = "{{{},{},{{}}}}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 16)
  }

  test("scoreStream ignores garbage") {
    val chars = "{<a>,<a>,<a>,<a>}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 1)
  }

  test("scoreStream ignores garbage and handles nesting") {
    val chars = "{{<ab>},{<ab>},{<ab>},{<ab>}}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 9)
  }

  test("scoreStream ignores garbage, handles nesting and double ignore characters") {
    val chars = "{{<!!>},{<!!>},{<!!>},{<!!>}}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 9)
  }

  test("scoreStream ignores garbage, handles nesting and single ignore characters") {
    val chars = "{{<a!>},{<a!>},{<a!>},{<ab>}}".toCharArray.toIterator
    assert(Day9.scoreStream(chars) == 3)
  }

  test("countGarbage handles empty garbage") {
    val chars = "<>".toCharArray.toIterator
    assert(Day9.countGarbage(chars) == 0)
  }

  test("countGarbage handles complicated garbage") {
    val chars = "<{o\"i!a,<{i<a>".toCharArray.toIterator
    assert(Day9.countGarbage(chars) == 10)
  }

  test("countGarbage handles groups etc also") {
    val chars = "{{}, <doop!>>, {<abc>}, <!!>}".toCharArray.toIterator
    assert(Day9.countGarbage(chars) == 7)
  }

}
