package Days

import scala.annotation.tailrec

object Day5 {

  @tailrec
  private def navigateMessage(jumps: Vector[Int], index: Int, count: Int, jumpRule: Int => Int): Int = {
    if (index < 0 || index >= jumps.size) count
    else {
      val jump = jumps(index)
      navigateMessage(jumps.updated(index, jumpRule(jump)), jump + index, count + 1, jumpRule)
    }
  }

  def countJumpsToExit(jumps: Vector[String]): Int = {
    navigateMessage(jumps.map(_.toInt), 0, 0, _ + 1)
  }

  def countStrangeJumpsToExit(jumps: Vector[String]): Int = {
    navigateMessage(jumps.map(_.toInt), 0, 0, i => if (i >= 3) i - 1 else i + 1)
  }
}
