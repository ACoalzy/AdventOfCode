package Days

object Day17 {

  def insertStep[A](s: A, list: List[A], index: Int, steps: Int): (List[A], Int) = {
    val newIndex = ((index + steps) % list.size) + 1
    val (start, end) = list.splitAt(newIndex)
    (start ++ List(s) ++ end, newIndex)
  }

  def spinlock(n: Int): Int = {
    val finalState = 1.to(2017).foldLeft(List(0), 0)((l, i) => insertStep(i, l._1, l._2, n))
    finalState._1(1)
  }

  case class State(size: Int, index: Int, first: Int)

  def superSpinlock(n: Int): Int = {
    1.to(50000000).foldLeft(State(1, 0, 0))((s, i) => {
      val newIndex = ((s.index + n) % s.size) + 1
      State(s.size+1, newIndex, if(newIndex == 1) i else s.first)
    }).first
  }

}
