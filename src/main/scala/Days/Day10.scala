package Days

import scala.annotation.tailrec

object Day10 {

  case class State(list: Seq[Int], position: Int, skip: Int) {
    def newPos(length: Int): Int = (length + position + skip) % list.size
  }

  def knot(length: Int, l: Seq[Int], x: Int): Seq[Int] = {
    if (length > l.size) l
    else {
      val slice = (l ++ l).slice(x, x + length).reverse
      val newX = x + length
      if (newX > l.size) {
        slice.takeRight(newX % l.size) ++ l.slice(newX % l.size, x) ++ slice.slice(0, slice.size - (newX % l.size))
      } else {
        l.slice(0, x) ++ slice ++ l.drop(newX)
      }
    }
  }

  def hash(lengths: List[Int], z: State): State =
    lengths.foldLeft(z)((s, i) => State(knot(i, s.list, s.position), s.newPos(i), s.skip + 1))

  def denseHash(lengths: List[Int], z: State): String = {
    val finalState = (1 to 64).foldLeft(z: State)((s: State, _) => hash(lengths ++ List(17, 31, 73, 47, 23), s))
    val denseHash = finalState.list.grouped(16).map(s => s.fold(0)(_ ^ _))
    denseHash.map("%02x".format(_)).mkString
  }
}
