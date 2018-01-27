package Days

import scala.annotation.tailrec

object Day22 {

  sealed trait Direction
  object Left extends Direction
  object Right extends Direction

  case class Point(x: Int, y: Int) {
    def +(p: Point) = Point(x + p.x, y + p.y)
  }

  case class Carrier(p: Point, v: Point) {
    def move(node: Char): Carrier = {
      val newV: Point = node match {
        case '.' => rotate(v, Left)
        case '#' => rotate(v, Right)
        case 'F' => Point(v.x * -1, v.y * -1)
        case _ => v
      }
      Carrier(p + newV, newV)
    }
  }

  def rotate(v: Point, dir: Direction): Point = dir match {
    case Left => Point(-v.y, v.x)
    case Right => Point(v.y, -v.x)
  }

  case class Map(map: Vector[Vector[Char]]) {
    def updated(p: Point, c: Char): Map = Map(map.updated(p.x, map(p.x).updated(p.y, c)))
  }

  object Map {
    def create(map: Vector[String]): Map = Map(map.map(_.toVector))
  }

  private def marker(node: Char): Char = node match {
    case '.' => 'W'
    case 'W' => '#'
    case '#' => 'F'
    case 'F' => '.'
  }

  private def counter(node: Char): Int = if (node == 'W') 1 else 0

  def burst(map: Map, count: Int, car: Carrier): (Map, Int, Carrier) = {
    val node = map.map(car.p.x)(car.p.y)
    (map.updated(car.p, marker(node)), count + counter(node), car.move(node))
  }

  def outOfBounds(newMap: Map, newCar: Carrier): Boolean =
    newCar.p.x < 0 || newCar.p.y < 0 || newCar.p.x >= newMap.map.size || newCar.p.y >= newMap.map.size

  def extend(map: Map, car: Carrier): (Map, Carrier) = {
    val row: Vector[Char] = Vector.fill(map.map.size + 2)('.')
    (Map(row +: map.map.map(v => '.' +: v :+ '.') :+ row), car.copy(p = car.p + Point(1,1)))
  }

  def countInfections(map: Vector[String], bursts: Int): Int = {
    @tailrec
    def go(map: Map, i: Int, count: Int, car: Carrier): Int = {
      if (i == bursts) count
      else {
        val (boundedMap, boundedCar) = if (outOfBounds(map, car)) extend(map, car) else (map, car)
        val (newMap, newCount, newCar) = burst(boundedMap, count, boundedCar)
        go(newMap , i + 1, newCount, newCar)
      }
    }

    go(Map.create(map), 0, 0, Carrier(Point(map.size/2, map.size/2), Point(-1, 0)))
  }

}
