package Days

object Day19 {

  case class Point(x: Int, y: Int) {
    def +(p: Point): Point = Point(x + p.x, y + p.y)
    def -(p: Point): Point = Point(x - p.x, y - p.y)
  }

  def findStart(l: String): Point = Point(0, l.indexOf('|'))

  private def adjacents(p: Point): List[Point] = List(Point(p.x+1, p.y), Point(p.x-1, p.y), Point(p.x, p.y+1), Point(p.x, p.y-1))

  def isPath(p: Point, list: Seq[String]): Boolean = {
    try {
      list(p.x)(p.y) != ' '
    } catch {
      case _: IndexOutOfBoundsException => false
    }
  }

  def findPath(pos: Point, dir: Point, lines: Seq[String]): Point = {
    adjacents(pos).filter(p => p != pos - dir && isPath(p, lines)).head
  }

  def outOfBounds(x: Int, size: Int): Boolean = x < 0 || x >= size

  def followPath(lines: Seq[String]): String = {
    def go(position: Point, direction: Point, path: String): String = {
      if (outOfBounds(position.x, lines.size) || outOfBounds(position.y, lines(position.x).size)) path
      else {
        val p = lines(position.x)(position.y)
        p match {
          case ' ' => path
          case '+' => {
            val newDirection = findPath(position, direction, lines) - position
            go(position + newDirection, newDirection, path + "+")
          }
          case c => go(position + direction, direction, path + c)
        }
      }
    }

    val start = findStart(lines(0))
    go(start, Point(1, 0), "")
  }

}
