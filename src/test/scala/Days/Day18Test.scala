package Days

import Days.Day18._
import org.scalatest.FunSuite

import scala.collection.immutable.Queue

class Day18Test extends FunSuite {

  test("create instruction handles send") {
    assert(Day18.createInstruction("snd X").get == Snd("X"))
  }

  test("create instruction handles set") {
    assert(Day18.createInstruction("set X Y").get == Set("X", "Y"))
  }

  test("create instruction handles add") {
    assert(Day18.createInstruction("add X Y").get == Add("X", "Y"))
  }

  test("create instruction handles mul") {
    assert(Day18.createInstruction("mul X Y").get == Mul("X", "Y"))
  }

  test("create instruction handles mod") {
    assert(Day18.createInstruction("mod X Y").get == Mod("X", "Y"))
  }

  test("create instructino handles rcv") {
    assert(Day18.createInstruction("rcv X").get == Rcv("X"))
  }

  test("create instruction handles jgz") {
    assert(Day18.createInstruction("jgz X 1").get == Jgz("X", "1"))
  }

  test("snd adds register value to State.play") {
    val startState = State(Map("a" -> 1), None, Queue(), 1)
    assert(Snd("a").implement(startState) == State(startState.registers, Some(1), Queue(), 2))
  }

  test("snd adds non register value to State.play") {
    val startState = State(Map("a" -> 1), None, Queue(), 1)
    assert(Snd("10").implement(startState) == State(startState.registers, Some(10), Queue(), 2))
  }

  test("set add adds register value to State.registers") {
    val startState = State(Map("a" -> 1, "b" -> 2), None, Queue(), 1)
    assert(Add("a", "b").implement(startState) == State(Map("a" -> 3, "b" -> 2), None, Queue(), 2))
  }

  test("set add adds non register value to State.registers") {
    val startState = State(Map("a" -> 1, "b" -> 2), None, Queue(), 1)
    assert(Add("a", "20").implement(startState) == State(Map("a" -> 21, "b" -> 2), None, Queue(), 2))
  }

  test("rcv removes first element in queue") {
    val startState = State(Map("a" -> 1), None, Queue(2), 1)
    assert(Rcv("a").implement(startState) == State(Map("a" -> 2), None, Queue(), 2))
  }

  test("rcv waits for value to read") {
    val startState = State(Map("a" -> 0), Some(1), Queue(), 1)
    assert(Rcv("a").implement(startState) == State(startState.registers, Some(1), Queue(), 1))
  }

  test("jgz updates next instruction by offset amount") {
    val startState = State(Map("a" -> 1), Some(1), Queue(), 1)
    assert(Jgz("a", "-2").implement(startState) == State(startState.registers, Some(1), Queue(), -1))
  }

  test("jgz doesn't update next instruction by offset amount if register is 0") {
    val startState = State(Map("a" -> 0), Some(1), Queue(), 1)
    assert(Jgz("a", "-2").implement(startState) == State(startState.registers, Some(1), Queue(), 2))
  }

  test("runPair handles example input") {
    assert(Day18.runPair(Seq("snd 1", "snd 2", "snd p", "rcv a", "rcv b", "rcv c", "rcv d")) == 3)
  }

  test("runPair with puzzle 1 input") {
    assert(Day18.runPair(Seq("set a 1", "add a 2", "mul a a", "mod a 5", "snd a", "set a 0", "rcv a", "jgz a -1", "set a 1", "jgz a -2")) == 1)
  }

}
