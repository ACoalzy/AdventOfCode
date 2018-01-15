package Days

import scala.io.Source

/**
  * Created by Adam on 14/01/2018.
  */
class Day1 {
  def decodeXAway(captcha: String, x: Int): Int = {
    val looped = captcha + captcha
    captcha.zipWithIndex.filter { case (c, i) => c == looped(i + x) }.map { case (c, _) => c.asDigit }.sum
  }
}

object Day1 extends App {
  val day1 = new Day1()
  val source = Source.fromResource("Days/Day1Captcha.txt").getLines()
  val captcha = source.next()

  println(day1.decodeXAway(captcha, 1))
  println(day1.decodeXAway(captcha, captcha.length / 2))
}
