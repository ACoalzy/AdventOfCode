package Days

import scala.annotation.tailrec

object Day9 {

  def scoreStream(chars: Iterator[Char]): Int = {
    @tailrec
    def go(chars: Iterator[Char], depth: Int, garbage: Boolean, escaped: Boolean, score: Int): Int = {
      if (chars.hasNext) {
        chars.next() match {
          case '{' if !escaped && !garbage => go(chars, depth + 1, false, false, score)
          case '}' if !escaped && !garbage => go(chars, depth - 1, false, false, score + depth)
          case '<' if !escaped => go(chars, depth, true, false, score)
          case '>' if !escaped => go(chars, depth, false, false, score)
          case '!' if !escaped => go(chars, depth, garbage, true, score)
          case _ => go(chars, depth, garbage, false, score)
        }
      } else score
    }

    go(chars, 0, false, false, 0)
  }

  def countGarbage(chars: Iterator[Char]): Int = {
    @tailrec
    def go(chars: Iterator[Char], garbage: Boolean, escaped: Boolean, score: Int): Int = {
      if (chars.hasNext) {
        chars.next() match {
          case '<' if !escaped && !garbage => go(chars, true, false, score)
          case '>' if !escaped => go(chars, false, false, score)
          case '!' if !escaped => go(chars, garbage, true, score)
          case _ if !escaped && garbage => go(chars, garbage, false, score + 1)
          case _ => go(chars, garbage, false, score)
        }
      } else score
    }

    go(chars, false, false, 0)
  }
}

/*

  {}

 */