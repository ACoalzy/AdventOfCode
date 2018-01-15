package Days

object Day2 {
  def checksumDifference(lines: Seq[String]):Int = {
    lines.map(digitSequence).map(difference).sum
  }

  private def digitSequence(s: String): Seq[Int] = {
    s.split("\\s+").filter(!_.isEmpty).map(_.toInt)
  }

  private def difference(digits: Seq[Int]): Int = {
    digits.max - digits.min
  }

  def checksumDivision(lines:Seq[String]): Int = {
    lines.map(digitSequence).map(evenDivision).sum
  }

  private def evenDivision(digits: Seq[Int]): Int = {
    val divides = for {
      x <- digits
      y <- digits
      z <- if (x != y && x % y == 0) Some(x / y) else None
    } yield (z)

    divides.sum
  }
}
