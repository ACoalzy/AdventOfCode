package Days

import scala.annotation.tailrec

object Day21 {

  case class Rule(matches: Set[Pattern], result: Pattern)

  case class Pattern(rows: List[String]) {
    def findMatches: Set[Pattern] = rotations(rows) ++ rotations(rows.reverse)

    private def split(size: Int): List[List[Pattern]] = {
      val splits = rows.grouped(rows.size / size).toList.map(l => l.map(_.grouped(rows.size / size).toList))
      val is = (0 to size-1).toList
      val splitPs = for (x <- is; y <- is) yield (Pattern(splits(x).map(_(y))))
      splitPs.grouped(size).toList
    }

    def split: List[List[Pattern]] = if (rows.size % 2 == 0) split(rows.size / 2) else split(rows.size / 3)
  }

  private def rotate(rows: List[String]): List[String] = rows.transpose.map(r => r.reverse.mkString)

  def rotations(rows: List[String]): Set[Pattern] = {
    @tailrec
    def go(acc: List[List[String]], rows: List[String]): List[Pattern] = {
      if (acc.contains(rows)) acc.map(Pattern(_))
      else go(rows :: acc, rotate(rows))
    }
    go(List(), rows).toSet
  }

  def parsePattern(s: String): Pattern = Pattern(s.split("/").toList)

  def parseRule(s: String): Rule = {
    val ps = s.split(" => ").map(parsePattern(_))
    Rule(ps(0).findMatches, ps(1))
  }

  def applyRule(rulePatterns: List[Rule], pattern: Pattern): Pattern = rulePatterns.find(r => r.matches.contains(pattern)).get.result

  private def patternsToStrings(ps: List[Pattern]): List[String] = ps.map(p => p.rows).reduce((x, y) => x.zip(y).map(ss => ss._1 + ss._2))

  def combine(ps: List[List[Pattern]]): Pattern = Pattern(ps.flatten(patternsToStrings(_)))

  def enhance(p: Pattern, rulePatterns: List[Rule]): Pattern = combine(p.split.map(_.map(applyRule(rulePatterns, _))))

  def enhanceXTimes(rules: List[String], x: Int): Pattern = 1.to(x).foldLeft(Pattern(List(".#.", "..#", "###")))((p, _) => enhance(p, rules.map(parseRule(_))))

  def countOnPixels(p: Pattern): Int = p.rows.flatten.filter(_ == '#').size
}
