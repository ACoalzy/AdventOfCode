package Days

object Day25 {

  abstract case class State(tape: Seq[Int], i: Int) {
    def transition: State = {
      val (tape2, i2) = adjustTape(tape, i)
      if (tape2(i2) == 0) optionA(tape2, i2)
      else optionB(tape2, i2)
    }

    def optionA(tape: Seq[Int], i: Int): State

    def optionB(tape: Seq[Int], i: Int): State

    protected def write(tape: Seq[Int], i: Int, value: Int): Seq[Int] = tape.updated(i, value)

    protected def adjustTape(tape: Seq[Int], i: Int) = if (i < 0) (0 +: tape, i + 1) else if (i >= tape.size) (tape :+ 0, i) else (tape, i)
  }

  class A(tape: Seq[Int], i: Int) extends State(tape, i) {
    override def optionA(tape: Seq[Int], i: Int): State = new B(write(tape, i, 1), i+1)

    override def optionB(tape: Seq[Int], i: Int): State = new F(write(tape, i, 0), i-1)
  }

  class B(tape: Seq[Int], i: Int) extends State(tape, i) {
    override def optionA(tape: Seq[Int], i: Int): State = new C(write(tape, i, 0), i+1)

    override def optionB(tape: Seq[Int], i: Int): State = new D(write(tape, i, 0), i+1)
  }

  class C(tape: Seq[Int], i: Int) extends State(tape, i) {
    override def optionA(tape: Seq[Int], i: Int): State = new D(write(tape, i, 1), i-1)

    override def optionB(tape: Seq[Int], i: Int): State = new E(write(tape, i, 1), i+1)
  }

  class D(tape: Seq[Int], i: Int) extends State(tape, i) {
    override def optionA(tape: Seq[Int], i: Int): State = new E(write(tape, i, 0), i-1)

    override def optionB(tape: Seq[Int], i: Int): State = new D(write(tape, i, 0), i-1)
  }

  class E(tape: Seq[Int], i: Int) extends State(tape, i) {
    override def optionA(tape: Seq[Int], i: Int): State = new A(write(tape, i, 0), i+1)

    override def optionB(tape: Seq[Int], i: Int): State = new C(write(tape, i, 1), i+1)
  }

  class F(tape: Seq[Int], i: Int) extends State(tape, i) {
    override def optionA(tape: Seq[Int], i: Int): State = new A(write(tape, i, 1), i-1)

    override def optionB(tape: Seq[Int], i: Int): State = new A(write(tape, i, 1), i+1)
  }

  def runMachineXSteps(x: Int): Seq[Int] = {
    1.to(x).foldLeft(new A(Vector(), 0): State)((s, _) => s.transition).tape
  }

}
