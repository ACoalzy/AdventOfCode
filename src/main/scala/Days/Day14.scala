package Days

import scala.annotation.tailrec

object Day14 {

  case class Coord(x: Int, y: Int)

  private def getHashes(key: String): Seq[String] = {
    val hashStrings = 0 to 127 map(key + "-" + _)
    val hashInputs = hashStrings.map(_.toList).map(_.map(_.toInt))
    hashInputs.map(Day10.denseHash(_, Day10.State(0 to 255, 0, 0)))
  }

  def countUsedBits(key: String): Int = {
    getHashes(key).map(BigInt(_, 16).toString(2).groupBy(identity)('1').size).sum
  }

  // assumes square
  def adjacents(c: Coord, maxIndex: Int): List[Coord] = {
    val up = if (c.x-1 >= 0) Some(Coord(c.x-1, c.y)) else None
    val down = if (c.x+1 < maxIndex) Some(Coord(c.x+1, c.y)) else None
    val left = if (c.y-1 >= 0) Some(Coord(c.x, c.y-1)) else None
    val right = if (c.y+1 < maxIndex) Some(Coord(c.x, c.y+1)) else None
    List(up, down, left, right).flatten
  }

  def fillGroup(c: Coord, binaries: Seq[String]): Set[Coord] = {
    def go(acc: Set[Coord], c: Coord): Set[Coord] = {
      if (acc.contains(c) || binaries(c.x)(c.y) == '0') acc
      else adjacents(c, binaries.size).foldLeft(acc + c)(go(_, _))
    }

    go(Set(), c)
  }

  def findGroup(c: Coord, groups: List[Set[Coord]], binaries: Seq[String]): List[Set[Coord]] = {
    if (binaries(c.x)(c.y) == '0' || groups.flatten.contains(c)) groups
    else {
      fillGroup(c, binaries) :: groups
    }
  }

  def countRegions(key: String): Int = {
    val binaries = getHashes(key).map(BigInt(_, 16).toString(2)).map(s => List.fill(128 - s.size)(0).mkString + s)
    val indexes = for (x <- 0 to 127; y <- 0 to 127) yield (Coord(x, y))
    val groups = indexes.foldLeft(List():List[Set[Coord]])((s, c) => findGroup(c, s, binaries))
    groups.size
  }

}
