package Days

import Days.Day8.{Condition, Instruction}
import org.scalatest.FunSuite

class Day8Test extends FunSuite {

  val exampleInput = List("b inc 5 if a > 1", "a inc 1 if b < 5", "c dec -10 if a >= 1", "c inc -20 if c == 10")

  test("build instructions handles test input") {
    assert(Day8.buildInstructions(exampleInput) == List(Instruction("b","inc",5,Condition("a",">",1)), Instruction("a","inc",1,Condition("b","<",5)), Instruction("c","dec",-10,Condition("a",">=",1)), Instruction("c","inc",-20,Condition("c","==",10))))
  }

  test("apply condition handles > greater") {
    assert(Day8.applyCondition(Condition("a", ">", 1), Map("a" -> 2)))
  }
  test("apply condition handles > equals") {
    assert(Day8.applyCondition(Condition("a", ">", 1), Map("a" -> 1)) == false)
  }
  test("apply condition handles > less") {
    assert(Day8.applyCondition(Condition("a", ">", 1), Map("a" -> 0)) == false)
  }

  test("apply condition handles >= greater") {
    assert(Day8.applyCondition(Condition("a", ">", 1), Map("a" -> 2)))
  }
  test("apply condition handles >= equals") {
    assert(Day8.applyCondition(Condition("a", ">=", 1), Map("a" -> 1)))
  }
  test("apply condition handles >= less") {
    assert(Day8.applyCondition(Condition("a", ">=", 1), Map("a" -> 0)) == false)
  }

  test("apply condition handles < greater") {
    assert(Day8.applyCondition(Condition("a", "<", 1), Map("a" -> 2)) == false)
  }
  test("apply condition handles < equals") {
    assert(Day8.applyCondition(Condition("a", "<", 1), Map("a" -> 1)) == false)
  }
  test("apply condition handles < less") {
    assert(Day8.applyCondition(Condition("a", "<", 1), Map("a" -> 0)))
  }

  test("apply condition handles <= greater") {
    assert(Day8.applyCondition(Condition("a", "<=", 1), Map("a" -> 2)) == false)
  }
  test("apply condition handles <= equals") {
    assert(Day8.applyCondition(Condition("a", "<=", 1), Map("a" -> 1)))
  }
  test("apply condition handles <= less") {
    assert(Day8.applyCondition(Condition("a", "<=", 1), Map("a" -> 0)))
  }

  test("apply condition handles == greater") {
    assert(Day8.applyCondition(Condition("a", "==", 1), Map("a" -> 2)) == false)
  }
  test("apply condition handles == equals") {
    assert(Day8.applyCondition(Condition("a", "==", 1), Map("a" -> 1)))
  }
  test("apply condition handles == less") {
    assert(Day8.applyCondition(Condition("a", "==", 1), Map("a" -> 0)) == false)
  }

  test("apply condition handles != greater") {
    assert(Day8.applyCondition(Condition("a", "!=", 1), Map("a" -> 2)))
  }
  test("apply condition handles != equals") {
    assert(Day8.applyCondition(Condition("a", "!=", 1), Map("a" -> 1)) == false)
  }
  test("apply condition handles != less") {
    assert(Day8.applyCondition(Condition("a", "!=", 1), Map("a" -> 0)))
  }

  test("apply inc instruction with valid condition") {
    assert(Day8.applyInstruction(Instruction("a", "inc", 1, Condition("a", "==", 1)), Map("a" -> 1)) == Map("a" -> 2))
  }

  test("apply inc instruction with invalid condition") {
    assert(Day8.applyInstruction(Instruction("a", "inc", 1, Condition("a", "!=", 1)), Map("a" -> 1)) == Map("a" -> 1))
  }

  test("apply dec instruction with valid condition") {
    assert(Day8.applyInstruction(Instruction("a", "dec", 1, Condition("a", "==", 1)), Map("a" -> 1)) == Map("a" -> 0))
  }

  test("apply dec instruction with invalid condition") {
    assert(Day8.applyInstruction(Instruction("a", "dec", 1, Condition("a", "!=", 1)), Map("a" -> 1)) == Map("a" -> 1))
  }

}
