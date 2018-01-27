package Days

import Days.Day23.Sub
import org.scalatest.FunSuite

class Day23Test extends FunSuite {

  test("create instruction handles set") {
    assert(Day23.createInstruction("set x y").get == Day23.Set("x", "y"))
  }

  test("create instruction handles sub") {
    assert(Day23.createInstruction("sub x y").get == Day23.Sub("x", "y"))
  }

  test("create instruction handles mul") {
    assert(Day23.createInstruction("mul x y").get == Day23.Mul("x", "y"))
  }

  test("create instruction handles jnz") {
    assert(Day23.createInstruction("jnz x y").get == Day23.Jnz("x", "y"))
  }

  test("check for mul increments count if instruction is mul") {
    assert(Day23.checkForMul(Day23.Mul("x", "y"), 1) == 2)
  }

  test("check for mul doesn't increments count if instruction is !mul") {
    assert(Day23.checkForMul(Day23.Set("x", "y"), 1) == 1)
  }

  test("optimise removes loops not affecting final value") {
    val instructions = List(Day23.Set("a", "1"), Sub("a", "1"), Day23.Set("h", "1"))
    assert(Day23.optimise(instructions) == List(Day23.Set("h", "1")))
  }

}
