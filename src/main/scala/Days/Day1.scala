package Days

import scala.io.Source

object Day1 extends App {
  def decodeXAway(captcha: String, x: Int): Int = {
    captcha.zipWithIndex.filter {
      case (c, i) => {
        val offset = Math.abs(((i + x) % captcha.length))
        c == captcha(offset)
      }
    }.map {
      case (c, _) => c.asDigit
    }.sum
  }
}
