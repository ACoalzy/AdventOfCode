package Days

import scala.collection.immutable.Queue

object Day18 {

  case class State(registers: Map[String, Long], play: Option[Long], rec: Queue[Long], ins: Int) {
    def updateRegisters(key: String, value: String)(f: (Long, Long) => Long): State = {
      this.copy(registers = registers.updated(key, f(registers(key), getValue(value))), ins = ins + 1)
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

  case class Snd(register: String) extends Instruction {
    override def implement(s: State): State = s.copy(play = Some(s.getValue(register)), ins = s.ins + 1)
  }

  case class Set(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)((_, y) => y)
  }

  case class Add(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)(_ + _)
  }

  case class Mul(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)(_ * _)
  }

  case class Mod(register: String, value: String) extends Instruction {
    override def implement(s: State): State = s.updateRegisters(register, value)(_ % _)
  }

  case class Rcv(register: String) extends Instruction {
    override def implement(s: State): State = {
      if (s.rec.size > 0) State(s.registers.updated(register, s.rec.head), s.play, s.rec.drop(1), s.ins + 1)
      else s
    }
  }

  case class Jgz(value: String, offset: String) extends Instruction {
    override def implement(s: State): State = {
      val nextInstruction = s.ins + (if (s.getValue(value) > 0) s.getValue(offset) else 1).toInt
      s.copy(ins = nextInstruction)
    }
  }

  private def parseLine(s: String): (String, String, String) = {
    val split = s.split(" ")
    (split(0), split(1), if (split.size > 2) split(2) else "")
  }

  def createInstruction(s: String): Option[Instruction] = {
    val (ins, x, y) = parseLine(s)
    ins match {
      case "snd" => Some(Snd(x))
      case "set" => Some(Set(x, y))
      case "add" => Some(Add(x, y))
      case "mul" => Some(Mul(x, y))
      case "mod" => Some(Mod(x, y))
      case "rcv" => Some(Rcv(x))
      case "jgz" => Some(Jgz(x, y))
      case _ => None
    }
  }

  private def runInstruction(s: State, ins: Seq[Instruction]): State = ins(s.ins).implement(s)

  private def stopped(s: State, limit: Int): Boolean = s.ins < 0 || s.ins >= limit

  private def updateQueue(a: State, b: State): State = {
    if (a.play.isEmpty && b.play.isEmpty) a
    else {
      val newQueue = if (b.play.isDefined) a.rec :+ b.play.get else a.rec
      a.copy(play = None, rec = newQueue)
    }
  }

  def runPair(ins: Seq[String]): Long = {
    val instructions = ins.map(createInstruction(_)).flatten

    def go(a: State, b: State, acc: Long): Long = {
      def conditionalImplement: State => State = s => if (stopped(s, instructions.size)) s else runInstruction(s, instructions)
      val aNext = conditionalImplement(a)
      val bNext = conditionalImplement(b)

      if (a.ins == aNext.ins && b.ins == bNext.ins) acc
      else go(updateQueue(aNext, bNext), updateQueue(bNext, aNext), acc + (if (bNext.play.isDefined) 1 else 0))
    }

    def startingMap(p: Long): Map[String, Long] = Map[String, Long]("p" -> p).withDefaultValue(0L)

    go(State(startingMap(0), None, Queue(), 0), State(startingMap(1), None, Queue(), 0), 0)
  }

}
