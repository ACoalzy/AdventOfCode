package Days

import Days.Day7.Node
import org.scalatest.FunSuite

class Day7Test extends FunSuite {

  test("buildNode with invalid input") {
    assert(Day7.buildNode("ribbit") == None)
  }

  test("buildNode handles nodes with no children") {
    assert(Day7.buildNode("abcd (60)") == Some("abcd" -> Node(60, Set())))
  }

  test("buildNode handles node with some children") {
    assert(Day7.buildNode("abcd (60) -> efgh, 1234") == Some("abcd" -> Node(60, Set("efgh", "1234"))))
  }

  test("buildTree handles all node types") {
    val correctTree = Map("abcd" -> Node(60, Set()), "efgh" -> Node(90, Set("xyz", "elf")))
    assert(Day7.buildTree(Seq("abcd (60)", "efgh (90) -> xyz, elf")) == correctTree)
  }

  test("example tree returns correct root") {
    val exampleTree = Map("pbga" -> Node(66, Set()), "xhth" -> Node(57, Set()), "ebii" -> Node(61, Set()), "havc" -> Node(66, Set()), "ktlj" -> Node(57, Set()), "fwft" -> Node(72, Set("ktlj", "cntj", "xhth")), "qoyq" -> Node(66, Set()), "padx" -> Node(45, Set("pbga", "havc", "qoyq")), "tknk" -> Node(41, Set("ugml", "padx", "fwft")), "jptl" -> Node(61, Set()), "ugml" -> Node(68, Set("gyxo", "ebii", "jptl")), "gyxo" -> Node(61, Set()), "cntj" -> Node(57, Set()))
    val testResult = Day7.findRoots(exampleTree)
    assert(testResult == Set("tknk"))
  }

  test("example tree returns correct weight to balance") {
    val exampleTree = Map("pbga" -> Node(66, Set()), "xhth" -> Node(57, Set()), "ebii" -> Node(61, Set()), "havc" -> Node(66, Set()), "ktlj" -> Node(57, Set()), "fwft" -> Node(72, Set("ktlj", "cntj", "xhth")), "qoyq" -> Node(66, Set()), "padx" -> Node(45, Set("pbga", "havc", "qoyq")), "tknk" -> Node(41, Set("ugml", "padx", "fwft")), "jptl" -> Node(61, Set()), "ugml" -> Node(68, Set("gyxo", "ebii", "jptl")), "gyxo" -> Node(61, Set()), "cntj" -> Node(57, Set()))
    assert(Day7.findRoots(exampleTree).map(r => Day7.findWeightImbalance(exampleTree, r)) == Set(Left(60)))
  }

}
