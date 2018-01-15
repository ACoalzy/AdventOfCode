package Days

import org.scalatest.FunSuite

class Day2Test extends FunSuite {

  test("checksumDifference empty seq returns 0") {
    assert(Day2.checksumDifference(Seq()) === 0)
  }

  test("checksumDifference strings of only one element return 0") {
    assert(Day2.checksumDifference(Seq("1", "2", "3")) === 0)
  }

  test("checksumDifference string of multiple elements returns difference") {
    assert(Day2.checksumDifference(Seq("1 2 33")) === 32)
  }

  test("checksumDifference multiple strings of multiple elements returns sum of differences") {
    assert(Day2.checksumDifference(Seq("3 1 2", "34 99 1", "100 101 99")) === 102)
  }

  test("checksumDifference handles strings with consecutive spaces") {
    assert(Day2.checksumDifference(Seq(" 1   2  ")) === 1)
  }

  test("checksumDivision empty seq returns 0") {
    assert(Day2.checksumDivision(Seq()) === 0)
  }

  test("checksumDivision single seq no even division returns 0") {
    assert(Day2.checksumDivision(Seq("2 3 5 7 11 13")) === 0)
  }

  test("checksumDivision single seq only even division same number returns 0") {
    assert(Day2.checksumDivision(Seq("2 3 5 7 11 5")) === 0)
  }

  test("checksumDivision multiple seq") {
    assert(Day2.checksumDivision(Seq("5 9 2 8", "9 4 7 3", "3 8 6 5")) === 9)
  }
}
