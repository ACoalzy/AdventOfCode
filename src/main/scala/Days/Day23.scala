package Days

object Day23 {

  case class State(registers: Map[String, Long], i: Int) {
    def updateRegisters(key: String, value: String)(f: (Long, Long) => Long): State = {
      State(registers.updated(key, f(registers(key), getValue(value))), i + 1)
    }

    def getValue(s: String): Long = {
      if (isAllDigits(s)) s.toLong
      else registers(s)
    }
  }

  private def isAllDigits(x: String) = x.forall(c => c.isDigit || c == '-')

  sealed trait Instruction {
    def implement(s: State): State
  }

  case class Set(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)((_, y) => y)
  }

  case class Sub(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)(_ - _)
  }

  case class Mul(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)(_ * _)
  }

  case class Jnz(value: String, offset: String) extends Instruction {
    override def implement(s: State): State = {
      val nextInstruction = s.i + (if (s.getValue(value) != 0) s.getValue(offset) else 1).toInt
      s.copy(i = nextInstruction)
    }
  }

  private def parseLine(s: String): (String, String, String) = {
    val split = s.split(" ")
    (split(0), split(1), split(2))
  }

  def createInstruction(s: String): Option[Instruction] = {
    val (ins, x, y) = parseLine(s)
    ins match {
      case "set" => Some(Set(x, y))
      case "sub" => Some(Sub(x, y))
      case "mul" => Some(Mul(x, y))
      case "jnz" => Some(Jnz(x, y))
      case _ => None
    }
  }

  private def stopped(s: State, limit: Int): Boolean = s.i < 0 || s.i >= limit

  def checkForMul(instruction: Instruction, count: Int): Int = instruction match {
    case Mul(_, _) => count + 1
    case _ => count
  }

  def countMuls(instructions: List[String]): Int = {
    val ins = instructions.map(createInstruction(_)).flatten

    def go(s: State, count: Int): Int = {
      if (stopped(s, ins.size)) count
      else go(ins(s.i).implement(s), checkForMul(ins(s.i), count))
    }

    go(State(Map().withDefaultValue(0), 0), 0)
  }

  def optimise(instructions: List[Instruction]): List[Instruction] = {
    instructions
  }

  def runProgram(instructions: List[String]): State = {
    val ins = optimise(instructions.map(createInstruction(_)).flatten)
    val initState = State(Map[String, Long]("a" -> 1).withDefaultValue(0), 0)

    def go(s: State): State = {
      if (stopped(s, ins.size)) s
      else go(ins(s.i).implement(s))
    }

    go(initState)
  }

  def isPrime(n: Int): Boolean = !((2 until n - 1) exists (n % _ == 0))

  // puzzle 2, manually optimised example input into code
  def countNonPrimesBetween(b: Int, c: Int, increment: Int): Int = {
    b.to(c).by(increment).foldLeft(0)((c, i) => c + (if (isPrime(i)) 0 else 1))
  }

}
