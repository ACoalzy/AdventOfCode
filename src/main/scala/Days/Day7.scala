package Days

import scala.collection.immutable.HashMap

object Day7 {

  case class Node(weight: Int, children: Set[String])

  def buildNode(details: String): Option[(String, Node)] = {
    if (!details.matches("\\w+ \\(\\d+\\)( -> \\w+(, \\w+)*)*")) None
    else {
      val split = details.split(" -> ")
      val nameWeightSplit = split(0).replace("(", "").replace(")", "").split(" ")
      val children = if (split.length > 1) split(1).split(", ").toSet else Set(): Set[String]

      Some(nameWeightSplit(0) -> Node(nameWeightSplit(1).toInt, children))
    }
  }

  def buildTree(nodeDetails: Seq[String]): Map[String, Node] = {
    nodeDetails.flatMap(s => buildNode(s)).toMap
  }

  def findRoots(tree: Map[String, Node]): Set[String] = {
    tree.keySet -- tree.values.flatMap(v => v.children).toSet
  }

  def sequence[A, B](l: List[Either[A, B]]): Either[A, List[B]] = l match {
    case Nil => Right(Nil)
    case h :: t => for (hh <- h; tt <- sequence(t)) yield (hh :: tt)
  }

  case class Pair(name: String, weight: Int)

  def findNodeWithWeightDifference(ws: List[Pair]): Option[Pair] = {
    val correctWeight = ws.groupBy(_.weight).maxBy(_._2.size)._1
    val badWeight = ws.find(_.weight != correctWeight)
    badWeight.map(n => Pair(n.name, correctWeight - n.weight))
  }

  def findWeightImbalance(tree: Map[String, Node], root: String): Either[Int, Pair] = {
    if (tree(root).children.size == 0) Right(Pair(root, tree(root).weight))
    else {
      val weights = sequence(tree(root).children.toSeq.map(findWeightImbalance(tree, _)).toList)

      weights.flatMap(ws => {
        val weightDiff = findNodeWithWeightDifference(ws)
        weightDiff match {
          case None => Right(Pair(root, ws.map(_.weight).sum + tree(root).weight))
          case Some(a) => Left(tree(a.name).weight + a.weight)
        }
      })
    }
  }
}