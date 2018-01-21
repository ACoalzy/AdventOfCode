package Days

import scala.annotation.tailrec

object Day12 {

  private def buildPipe(str: String): (String, Set[String]) = {
    val split = str.split(" <-> ")
    split(0) -> split(1).split(", ").toSet
  }

  def buildPipes(pipes: List[String]): Map[String, Set[String]] = {
    pipes.map(buildPipe(_)).toMap
  }

  private def getPipesInGroup(pipes: Map[String, Set[String]], position: String, acc: Set[String]): Set[String] = {
    val links = pipes(position).filter(!acc.contains(_))
    if (links.size == 0) acc
    else {
      links.foldLeft(acc ++ links)((l, s) => l ++ getPipesInGroup(pipes, s, l))
    }
  }

  def countPipesInGroup(pipes: Map[String, Set[String]], start: String): Int = {
    getPipesInGroup(pipes, start, Set(start)).size
  }

  def countPipeGroups(pipes: Map[String, Set[String]]): Int = {
    def go(pipes: Map[String, Set[String]], acc: Int): Int = {
      if (pipes.isEmpty) acc
      else {
        val start = pipes.keySet.toList.head
        go(pipes -- getPipesInGroup(pipes, start, Set(start)), acc + 1)
      }
    }

    go(pipes, 0)
  }

}
