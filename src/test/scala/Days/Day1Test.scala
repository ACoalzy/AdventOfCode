package Days

import org.scalatest.FunSuite

class Day1Test extends FunSuite {
  test("empty string returns 0") {
    assert(Day1.decodeXAway("", 0) === 0)
  }

  test("unique string returns 0") {
    assert(Day1.decodeXAway("1234567", 1) === 0)
  }

  test("single char string returns 1") {
    assert(Day1.decodeXAway("1", 1) === 1)
  }

  test("single char string returns int if 0 as X") {
    assert(Day1.decodeXAway("1", 0) === 1)
  }

  test("unique string returns sum if 0 as X") {
    assert(Day1.decodeXAway("12345", 0) === 15)
  }

  test("matching check will loop") {
    assert(Day1.decodeXAway("121", 1) === 1)
  }

  test("handles X > string length") {
    assert(Day1.decodeXAway("121416", 14) === 3)
  }

  test("negative numbers are handled same as positive") {
    assert(Day1.decodeXAway("1122", -1) === 3)
  }

}
