package Days

import Days.Day24.Component
import org.scalatest.FunSuite

class Day24Test extends FunSuite {

  test("strongest bridge with only one arrangement") {
    val components = Vector("0/2", "2/3")
    assert(Day24.strongestBridge(components) == Day24.Bridge(List(Component(0, 2), Component(2, 3))))
  }

  test("strongest bridge with unconnected pieces") {
    val components = Vector("0/2", "2/3", "4/5", "10/1")
    assert(Day24.strongestBridge(components) == Day24.Bridge(List(Component(0, 2), Component(2, 3))))
  }

  test("strongest bridge with example input") {
    val components = Vector("0/2", "2/2", "2/3", "3/4", "3/5", "0/1", "10/1", "9/10")
    assert(Day24.strongestBridge(components) == Day24.Bridge(List(Component(0, 1), Component(10, 1), Component(9, 10))))
  }

  test("longest strongest with example input") {
    val components = Vector("0/2", "2/2", "2/3", "3/4", "3/5", "0/1", "10/1", "9/10")
    assert(Day24.longestStrongestBridge(components) == Day24.Bridge(List(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 5))))
  }

}
