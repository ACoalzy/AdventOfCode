package Days

object Day11 {

  def getShortestDistance(steps: Map[String, Int]): Int = {
    val stepsWithDefault = steps.withDefaultValue(0)
    val x = stepsWithDefault("n") + stepsWithDefault("ne") - (stepsWithDefault("s") + stepsWithDefault("sw"))
    val y = stepsWithDefault("nw") + stepsWithDefault("sw") - (stepsWithDefault("ne") + stepsWithDefault("se"))
    val z = stepsWithDefault("s") + stepsWithDefault("se") - (stepsWithDefault("n") + stepsWithDefault("nw"))
    (math.abs(x) + math.abs(y) + math.abs(z)) / 2
  }

  private def groupSteps(steps: List[String]): Map[String, Int] = steps.groupBy(identity).mapValues(_.size)

  def shortestPathLength(steps: List[String]): Int = {
    val totalSteps = groupSteps(steps)
    getShortestDistance(totalSteps)
  }

  def getFurthestEverAway(steps: List[String]): Int = {
    steps.scanLeft(List(): List[String])((l, s) => s :: l).map(l => getShortestDistance(groupSteps(l))).max
  }

}
