package Days

import sun.corba.Bridge

object Day24 {

  case class Component(x: Int, y: Int)

  case class Bridge(components: List[Component]) {
    def strength: Int = components.map(c => c.x + c.y).sum
    def length: Int = components.size
  }

  private def parseComponent(str: String): Component = {
    val split = str.split("/")
    Component(split(0).toInt, split(1).toInt)
  }

  def createComponents(list: Seq[String]): Seq[Component] = list.map(parseComponent(_))

  def newJoin(c: Component, join: Int): Int = if (join == c.x) c.y else c.x

  def strongestBridge(input: Seq[String]): Bridge = {
    def strongest(components: Seq[Component], join: Int): Bridge = {
      val choices = components.filter(c => c.x == join || c.y == join)
      if (choices.size > 0) choices.map(c => Bridge(c :: strongest(components.filterNot(_ == c), newJoin(c, join)).components)).maxBy(_.strength)
      else Bridge(Nil)
    }

    strongest(createComponents(input), 0)
  }

  def longerOrStronger(a: Bridge, b: Bridge): Bridge = {
    if (a.length == b.length) List(a, b).maxBy(_.strength)
    else List(a, b).maxBy(_.length)
  }

  def longestStrongestBridge(input: Seq[String]): Bridge = {
    def longestStrongest(components: Seq[Component], join: Int): Bridge = {
      val choices = components.filter(c => c.x == join || c.y == join)
      if (choices.size > 0) choices.map(c => Bridge(c :: longestStrongest(components.filterNot(_ == c), newJoin(c, join)).components)).reduce(longerOrStronger)
      else Bridge(Nil)
    }

    longestStrongest(createComponents(input), 0)
  }

}
