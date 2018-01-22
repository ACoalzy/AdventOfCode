package Days

import Days.Day15.Generator
import org.scalatest.FunSuite

class Day15Test extends FunSuite {

  test("generate new value with first 3 A values") {
    assert(Generator(65, 16807).nextGenerator.nextGenerator.nextGenerator.value == 245556042)
  }

  test("generate new value with first 3 B values") {
    assert(Generator(8921, 48271).nextGenerator.nextGenerator.nextGenerator.value == 1431495498)
  }

  test("judge matches 245556042 and 1431495498") {
    assert(Day15.judge(245556042, 1431495498) == 1)
  }

  test("judge doesn't match 1181022009 and 1233683848") {
    assert(Day15.judge(1181022009, 1233683848) == 0)
  }

  test("count X generation matches with example input") {
    assert(Day15.countXGenerationMatches(Generator(65, 16807), Generator(8921, 48271), 5) == 1)
  }

  test("count X generation matches with example input 40,000,000") {
    assert(Day15.countXGenerationMatches(Generator(65, 16807), Generator(8921, 48271), 40000000L) == 588)
  }

  test("count X generations matches with example input for gen multiples active") {
    assert(Day15.countXGenerationMatches(Generator(65, 16807, 4), Generator(8921, 48271, 8), 1056) == 1)
  }

  test("count X generations is zero for one less iteration than example") {
    assert(Day15.countXGenerationMatches(Generator(65, 16807, 4), Generator(8921, 48271, 8), 1055) == 0)
  }

  test("count X generations matches with example input for gen multiples active 5000000") {
    assert(Day15.countXGenerationMatches(Generator(65, 16807, 4), Generator(8921, 48271, 8), 5000000) == 309)
  }

}
