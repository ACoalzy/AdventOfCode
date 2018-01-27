package Days

import org.scalatest.FunSuite

class Day25Test extends FunSuite {

  test("run machine x times runs one step") {
    assert(Day25.runMachineXSteps(1) == Vector(1))
  }

  test("run machine x times runs 5 steps") {
    assert(Day25.runMachineXSteps(5) == Vector(1, 0, 1))
  }

  test("run machine x times runs 12794428 steps") {
    assert(Day25.runMachineXSteps(12794428).filter(_ == 1).size == 2832)
  }

}
