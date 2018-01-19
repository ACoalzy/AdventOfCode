package Days

object Day8 {

  case class Condition(register: String, rule: String, value: Int)

  case class Instruction(register: String, rule: String, amount: Int, condition: Condition)

  /**
    * Creates list of instructions from strings, assumes valid input
    * @param lines
    * @return
    */
  def buildInstructions(lines: List[String]): List[Instruction] = {
    lines.map(l => {
      val modCondSplit = l.split(" if ")
      val modPieces = modCondSplit(0).split(" ")
      val condPieces = modCondSplit(1).split(" ")
      Instruction(modPieces(0), modPieces(1), modPieces(2).toInt, Condition(condPieces(0), condPieces(1), condPieces(2).toInt))
    })
  }

  def applyCondition(condition: Condition, m: Map[String, Int]): Boolean = {
    condition.rule match {
      case ">" => m(condition.register) > condition.value
      case ">=" => m(condition.register) >= condition.value
      case "<" => m(condition.register) < condition.value
      case "<=" => m(condition.register) <= condition.value
      case "==" => m(condition.register) == condition.value
      case "!=" => m(condition.register) != condition.value
    }
  }

  def applyInstruction(i: Instruction, m: Map[String, Int]): Map[String, Int] = {
    if (applyCondition(i.condition, m)) {
      i.rule match {
        case "inc" => m.updated(i.register, m(i.register) + i.amount)
        case "dec" => m.updated(i.register, m(i.register) - i.amount)
        case _ => m
      }
    } else m
  }

  def runInstructions(instructions: List[Instruction]): Map[String, Int] = {
    val default: Map[String, Int] = Map().withDefault(_ => 0)
    instructions.foldLeft(default)((m, i) => applyInstruction(i, m))
  }

  def runInstructionsFullHistory(instructions: List[Instruction]): List[Map[String, Int]] = {
    val default: Map[String, Int] = Map().withDefault(_ => 0)
    instructions.scanLeft(default)((m, i) => applyInstruction(i, m))
  }
}
