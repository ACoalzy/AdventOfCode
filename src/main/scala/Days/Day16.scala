package Days

object Day16 {

  sealed trait Move

  case class Spin(n: Int) extends Move

  case class Exchange(x: Int, y: Int) extends Move

  case class Partner(a: Char, b: Char) extends Move

  def parseMove(s: String): Option[Move] = {
    if (s.startsWith("s")) {
      Some(Spin(s.drop(1).toInt))
    } else if (s.startsWith("x")) {
      val programs = s.drop(1).split("/")
      Some(Exchange(programs(0).toInt, programs(1).toInt))
    } else if (s.startsWith("p")) {
      Some(Partner(s(1), s(3)))
    } else None
  }

  def swap(x: Int, y: Int, s: String): String = {
    s.updated(x, s(y)).updated(y, s(x))
  }

  def applyMove(p: String, move: Move): String = move match {
    case Spin(n) => p.drop(p.size - n) + p.take(p.size - n)
    case Exchange(x, y) => swap(x, y, p)
    case Partner(a, b) => swap(p.indexOf(a), p.indexOf(b), p)
  }

  def dance(moves: List[String], start: String): String = moves.map(parseMove(_)).flatten.foldLeft(start)(applyMove(_, _))

  def justOneDance(moves: List[String], n: Int): String = {
    val letters = 'a'.to('z').take(n).mkString
    dance(moves, letters)
  }

  def xDances(moves: List[String], n: Int, x: Long): String = {
    def getLoopSize(history: Set[String], i: Long, curr: String): Long = {
      val next = dance(moves, curr)
      if (history.contains(next) || i == x) i
      else getLoopSize(history + next, i + 1, next)
    }

    val letters = 'a'.to('z').take(n).mkString
    val loopSize = getLoopSize(Set(), 0, letters)
    val actualDanceNum = if (loopSize == x) x else x % loopSize
    1L.to(actualDanceNum).foldLeft(letters)((l, _) => dance(moves, l))
  }

}
