package Days

object Day15 {

  case class State(sum: Long, a: Generator, b: Generator)

  case class Generator(value: Long, multiplier: Long, multiple: Int = 1) {
    def generateNewValue(value: Long, multiplier: Long): Long = (value * multiplier) % 2147483647L

    def nextGenerator: Generator = Generator(generateNewValue(value, multiplier), multiplier, multiple)

    def isValid: Boolean = value % multiple == 0

    def nextValidGenerator: Generator = {
      def go(gen: Generator): Generator = if (gen.isValid) gen else go(gen.nextGenerator)
      go(nextGenerator)
    }
  }

  def judge(a: Long, b: Long): Long = {
    val mask: Long = (1 << 16) - 1
    if ((a & mask) == (b & mask)) 1
    else 0
  }

  def countXGenerationMatches(a: Generator, b: Generator, n: Long): Long = {
    0L.to(n-1).foldLeft(State(0, a, b))((state, _) => {
      val na = state.a.nextValidGenerator
      val nb = state.b.nextValidGenerator
      State(state.sum + judge(na.value, nb.value), na, nb)
    }).sum
  }
}
