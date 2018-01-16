package Days

object Day4 {

  def uniquePassphrase(s: String): Boolean = {
    val words = s.split("\\s+").toSeq
    words.distinct.size == words.size
  }

  def isAnagram(x: String, y: String): Boolean = x.sorted == y.sorted

  def noAnagramPassphrase(s: String): Boolean = {
    val words = s.split("\\s+").toSeq
    words.map(_.sorted).distinct.size == words.size
  }

  def countUniquePassphrases(phrases: Seq[String]): Int = {
    phrases.foldLeft(0)((i, s) => i + (if (uniquePassphrase(s)) 1 else 0))
  }

  def countNoAnagramPassphrases(phrases: Seq[String]): Int = {
    phrases.foldLeft(0)((i, s) => i + (if (noAnagramPassphrase(s)) 1 else 0))
  }
}
