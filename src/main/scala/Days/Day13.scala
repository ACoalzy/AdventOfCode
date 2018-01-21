package Days

import scala.annotation.tailrec

object Day13 {

  def buildLayers(l: List[String]): Map[Int, Int] = {
    l.map(_.split(": ")).map(x => (x(0).toInt, x(1).toInt)).toMap
  }

  private def rangeModulus(range: Int) = ((range - 1) * 2) max 1

  private def detectCollision(i: Int, r: Int, offset: Int): Option[Int] =
    if ((i + offset) % rangeModulus(r) == 0) Some(i * r) else None

  def tripSeverity(layers: Map[Int, Int], offset: Int): Option[Int] = {
    val severities = layers.map(i => detectCollision(i._1, i._2, offset)).flatten

    if (severities.size > 0) {
      Some(severities.sum)
    } else None
  }

  def minDelay(layers: Map[Int, Int]): Int = {
    @tailrec
    def go(acc: Int): Int = {
      tripSeverity(layers, acc) match {
        case None => acc
        case Some(_) => go(acc + 1)
      }
    }

    go(0)
  }
}
