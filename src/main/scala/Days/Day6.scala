package Days

import scala.annotation.tailrec

object Day6 {

  case class Memory(bs: Seq[Int])

  private case class Pair[A](value: A, index: Int)

  private def pairify[A](s: Seq[A]): Seq[Pair[A]] = s.zipWithIndex.map(i => Pair(i._1, i._2))

  def redistributionCycle(banks: Memory): Memory = {
    val indexed = pairify(banks.bs)
    val bank = indexed.foldLeft(Pair(0, 0))((z, i) => if (i.value > z.value) i else z)
    val bankSize = banks.bs.size
    val sharedIncrement = bank.value / bankSize
    val bonusLimit = bank.index + (bank.value % bankSize)

    Memory(indexed.map(p => {
      val alteredIndex = if (p.index < bank.index) p.index + bankSize else p.index
      if (alteredIndex == bank.index) sharedIncrement
      else if (alteredIndex <= bonusLimit) p.value + sharedIncrement + 1
      else p.value + sharedIncrement
    }))
  }

  @tailrec
  private def redistributeWhilstNoRepeat(banks: Memory, history: List[Memory], repeats: Int): List[Memory] = {
    if (history.contains(banks)) banks :: history
    else {
      val redistributedBanks = redistributionCycle(banks)
      redistributeWhilstNoRepeat(redistributedBanks, banks :: history, repeats +1)
    }
  }

  def countRedistributionCycleTimeToFindLoop(banks: Seq[Int]): Int = redistributeWhilstNoRepeat(Memory(banks), List(), 0).size-1

  def countRedistributionCycleLoopSize(banks: Seq[Int]): Int = {
    val history = redistributeWhilstNoRepeat(Memory(banks), List(), 0)
    val twoMatchingBanks = pairify(history).filter(p => p.value == history.head)
    twoMatchingBanks(1).index - twoMatchingBanks(0).index
  }
}
