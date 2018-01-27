import Days._

import scala.io.Source

object DayRunner extends App {
  println(Day25.runMachineXSteps(12794428).filter(_ == 1).size)
}
