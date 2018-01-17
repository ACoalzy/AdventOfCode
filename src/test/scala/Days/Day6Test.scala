package Days

import Days.Day6.Memory
import org.scalatest.FunSuite

class Day6Test extends FunSuite {

  test("redistributionCycle handles single element list") {
    assert(Day6.redistributionCycle(Memory(Seq(1))) == Memory(Seq(1)))
  }

  test("redistributionCycle handles multiple elements") {
    assert(Day6.redistributionCycle(Memory(Seq(3, 0, 0, 0))) == Memory(Seq(0, 1, 1, 1)))
  }

  test("redistributionCycle handles wrapping") {
    assert(Day6.redistributionCycle(Memory(Seq(0, 0, 0, 3, 0))) == Memory(Seq(1, 1, 0, 0, 1)))
  }

  test("redistributionCycle handles values larger than list size") {
    assert(Day6.redistributionCycle(Memory(Seq(5, 0, 0))) == Memory(Seq(1, 2, 2)))
  }

  test("redistributionCycle handles choosing largest bank") {
    assert(Day6.redistributionCycle(Memory(Seq(5, 4, 3))) == Memory(Seq(1, 6, 5)))
  }

  test("redistributionCycle chooses earliest highest bank") {
    assert(Day6.redistributionCycle(Memory(Seq(5, 4, 5, 4, 5))) == Memory(Seq(1, 5, 6, 5, 6)))
  }

  test("countRedistributionCycleTimeToFindLoop with example input") {
    assert(Day6.countRedistributionCycleTimeToFindLoop(Seq(0, 2, 7, 0)) == 5)
  }

  test("countRedistributionCycleLoopSize with example input") {
    assert(Day6.countRedistributionCycleLoopSize(Seq(0, 2, 7, 0)) == 4)
  }

}
