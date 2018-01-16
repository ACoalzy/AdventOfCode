package Days

import org.scalatest.FunSuite

class Day5Test extends FunSuite {

  test("count jumps to exit returns 0 for empty seq") {
    assert(Day5.countJumpsToExit(Vector()) == 0)
  }

  test("count jumps to exit returns 10 for 0 0 0 0 0") {
    assert(Day5.countJumpsToExit(Vector("0", "0", "0", "0", "0")) == 10)
  }

  test("count jumps to exit handles going backwards") {
    assert(Day5.countJumpsToExit(Vector("0", "0", "0", "0", "-4")) == 14)
  }

  test("count strange jumps example test") {
    assert(Day5.countStrangeJumpsToExit(Vector("0", "3", "0", "1", "-3")) == 10)
  }

}
