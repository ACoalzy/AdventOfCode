import Days._

import scala.io.Source

object DayRunner extends App {
  val lines = Source.fromResource("Days/Day7Tower.txt").getLines()

  val tree = Day7.buildTree(lines.toSeq)
  println(Day7.findRoots(tree).map(r => Day7.findWeightImbalance(tree, r)))
}
