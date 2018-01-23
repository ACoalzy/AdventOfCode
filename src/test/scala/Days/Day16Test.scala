package Days

import Days.Day16.{Exchange, Partner, Spin}
import org.scalatest.FunSuite

class Day16Test extends FunSuite {

  test("parse move handles spin") {
    assert(Day16.parseMove("s12") == Some(Spin(12)))
  }

  test("parse move handles exchange") {
    assert(Day16.parseMove("x15/3") == Some(Exchange(15, 3)))
  }

  test("parse move handles partner") {
    assert(Day16.parseMove("pb/e") == Some(Partner('b', 'e')))
  }

  test("parse move handles invalid") {
    assert(Day16.parseMove("f2") == None)
  }

  test("swap swaps two indexes in string") {
    assert(Day16.swap(1, 3, "abcde") == "adcbe")
  }

  test("apply move handles spin") {
    assert(Day16.applyMove("abcde", Spin(3)) == "cdeab")
  }

  test("apply move handles exchange") {
    assert(Day16.applyMove("abcde", Exchange(1, 3)) == "adcbe")
  }

  test("apply move handles partner") {
    assert(Day16.applyMove("abcde", Partner('c', 'e')) == "abedc")
  }

  test("xDances with example input") {
    assert(Day16.xDances(List("s1", "x3/4", "pe/b"), 5, 2) == "ceadb")
  }

}
