package Days

import org.scalatest.FunSuite

class Day12Test extends FunSuite {

  test("build pipes handles 1 join") {
    assert(Day12.buildPipes(List("0 <-> 2")) == Map("0" -> Set("2")))
  }

  test("build pipes handles multiple joins") {
    assert(Day12.buildPipes(List("0 <-> 2, 3, 4")) == Map("0" -> Set("2", "3", "4")))
  }

  test("count pipes in group handles pipe pointing to self") {
    val pipes = Day12.buildPipes(List("1 <-> 1"))
    assert(Day12.countPipesInGroup(pipes, "1") == 1)
  }

  test("count pipes in group handles example input") {
    val pipes = Day12.buildPipes(List("0 <-> 2", "1 <-> 1", "2 <-> 0, 3, 4", "3 <-> 2, 4", "4 <-> 2, 3, 6", "5 <-> 6", "6 <-> 4, 5"))
    assert(Day12.countPipesInGroup(pipes, "0") == 6)
  }

  test("count pipe groups handles 1 group") {
    val pipes = Day12.buildPipes(List("0 <-> 1", "1 <-> 0"))
    assert(Day12.countPipeGroups(pipes) == 1)
  }

  test("count pipe groups handles example input") {
    val pipes = Day12.buildPipes(List("0 <-> 2", "1 <-> 1", "2 <-> 0, 3, 4", "3 <-> 2, 4", "4 <-> 2, 3, 6", "5 <-> 6", "6 <-> 4, 5"))
    assert(Day12.countPipeGroups(pipes) == 2)
  }

}
