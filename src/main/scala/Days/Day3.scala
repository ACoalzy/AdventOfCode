package Days

object Day3 {

  case class Point(x: Int, y: Int)

  case class Square(p: Point, value: Int)

  def calculateSquareWidth(start: Int): Int = Stream.from(1).takeWhile(i => i * i < start).size + 1

  def get1Index(squareWidth: Int): Int = {
    if (squareWidth % 2 == 0) {
      (squareWidth / 2) - 1
    } else {
      squareWidth / 2
    }
  }

  def getStartIndex(start: Int): Point = {
    def square(x: Int) = x * x

    val squareWidth = calculateSquareWidth(start)
    val squared = square(squareWidth)
    val lowerSquared = square(squareWidth - 1)

    // if even width we know either top or right
    // else we know either bottom or left
    if (squareWidth % 2 == 0) {
      // bottom
      if (start + squareWidth > squared) {
        Point(squared - start, squareWidth - 1)
        // left
      } else {
        Point(squareWidth - 1, (start - (lowerSquared + 1)))
      }
    } else {
      // top
      if (start + squareWidth > squared) {
        Point((squareWidth - 1) - (squared - start), 0)
        // right
      } else {
        Point(0, squareWidth - (start - lowerSquared))
      }
    }
  }

  def manhattanDistance(start: Point, end: Point): Int = {
    Math.abs(start.x - end.x) + Math.abs(start.y - end.y)
  }

  def spiralManhattan(start: Int): Int = {
    val middle = get1Index(calculateSquareWidth(start))
    manhattanDistance(getStartIndex(start), Point(middle, middle))
  }

  def evenPower(i: Int): Boolean = {
    val sqrt = math.sqrt(i)
    sqrt % 1 == 0 && sqrt % 2 == 0
  }

  def appendSpiral(s: Option[Set[Square]], i: Int): (Option[Set[Square]]) = s.map(
    spiral => if (evenPower(i-1)) spiral.map(s => Square(Point(s.p.x + 1, s.p.y + 1), s.value)) else spiral
  ).map (
    spiral => {
      val index = getStartIndex(i)
      val values = for {
        x <- -1 to 1
        y <- -1 to 1
        z <- spiral.find(p => p.p.x == index.x + x && p.p.y == index.y + y)
      } yield (z.value)
      spiral + Square(index, Math.max(values.sum, 1))
    })

  def getMaxSquare(spiral: Option[Set[Square]], default: Int): Int = spiral match {
    case None => default
    case Some(a) => a.map(s => s.value).max
  }

  def spiralSumming(limit: Int): Int = {
    Stream.range(1, limit+1).foldLeft((0, Some(Set(): Set[Square]): Option[Set[Square]]))((z, i) => {
      if (z._1 > limit) (z._1, None)
      else {
        val spiral = appendSpiral(z._2, i)
        val max = getMaxSquare(spiral, z._1)
        (max, spiral)
      }
    })._1
  }
}
