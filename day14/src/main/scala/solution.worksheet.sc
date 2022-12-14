import scala.util.parsing.combinator._
import scala.annotation.tailrec

val input = scala.io.Source.fromFile("input.txt").mkString

// Input format:
// 498,4 -> 498,6 -> 496,6
// 503,4 -> 502,4 -> 502,9 -> 494,9

case class Coord(x: Int, y: Int)
case class Lines(vertices: List[Coord]) {
    private def iterBetween(a: Coord, b: Coord): List[Coord] = {
        if (a == b) {
            List(a)
        } else if (a.x == b.x) {
            val min = Math.min(a.y, b.y)
            val max = Math.max(a.y, b.y)
            (min to max).map(Coord(a.x, _)).toList
        } else {
            val min = Math.min(a.x, b.x)
            val max = Math.max(a.x, b.x)
            (min to max).map(Coord(_, a.y)).toList
        }
    }

    def points: List[Coord] = {
        @tailrec
        def iter(vertices: List[Coord], acc: List[Coord]): List[Coord] = {
            vertices match {
                case Nil => acc
                case a :: Nil => acc
                case a :: b :: tail => iter(b :: tail, acc ++ iterBetween(a, b))
            }
        }

        iter(vertices, Nil)
    }
}

// separated by a blank line
type Problem = List[Lines]

object Parser extends RegexParsers with PackratParsers {
    val number: PackratParser[Int] = """\d+""".r ^^ { _.toInt }
    val coord: PackratParser[Coord] = number ~ "," ~ number ^^ { case x ~ "," ~ y => Coord(x, y) }
    val lines: PackratParser[Lines] = rep1sep(coord, "->") ^^ { case points => Lines(points) }
    val problem: PackratParser[Problem] = repsep(lines, "")
}

val problem = Parser.parseAll(Parser.problem, input).get

sealed trait Cell
case object Air extends Cell
case object Rock extends Cell
case object Sand extends Cell

val startX = 500
val startY = 0

val minX = Math.min(problem.map(_.vertices.map(_.x).min).min, startX)
val maxX = Math.max(problem.map(_.vertices.map(_.x).max).max, startX)
val minY = Math.min(problem.map(_.vertices.map(_.y).min).min, startY)
val maxY = Math.max(problem.map(_.vertices.map(_.y).max).max, startY)

val initialMap = {
    var map: Map[Coord, Cell] = Map.empty

    for (y <- minY to maxY) {
        for (x <- minX to maxX) {
            map += (Coord(x, y) -> Air)
        }
    }

    for (lines <- problem) {
        for (point <- lines.points) {
            map += (point -> Rock)
        }
    }

    map
}

def printMap(map: Map[Coord, Cell]): Unit = {
    val minX = map.keys.map(_.x).min
    val maxX = map.keys.map(_.x).max
    val minY = map.keys.map(_.y).min
    val maxY = map.keys.map(_.y).max

    for (y <- minY to maxY) {
        for (x <- minX to maxX) {
            val cell = map.getOrElse(Coord(x, y), Air)
            val c = cell match {
                case _ if x == startX && y == startY => '+'
                case Air => '.'
                case Rock => '#'
                case Sand => 'o'
            }
            print(c)
        }
        println()
    }
}

printMap(initialMap)

@tailrec
final def spawnSand(map: Map[Coord, Cell], coord: Coord): Option[Map[Coord, Cell]] = {
    val bellow = Coord(coord.x, coord.y + 1)
    val bellowLeft = Coord(coord.x - 1, coord.y + 1)
    val bellowRight = Coord(coord.x + 1, coord.y + 1)

    (map.get(bellow), map.get(bellowLeft), map.get(bellowRight)) match {
        case (Some(Air), _, _) => spawnSand(map, bellow)
        case (_, Some(Air), _) => spawnSand(map, bellowLeft)
        case (_, _, Some(Air)) => spawnSand(map, bellowRight)
        case (None, _, _) | (_, None, _) | (_, _, None) => None
        case _ => Some(map + (coord -> Sand))
    }
}

val part1 = {
    var map: Option[Map[Coord, Cell]] = Some(initialMap)
    var count = -1

    while (map.isDefined) {
        map = spawnSand(map.get, Coord(startX, startY))
        //map.foreach(printMap)
        count += 1
    }

    count
}

// part 2

val floor = maxY + 2


@tailrec
final def spawnSandPart2(map: Map[Coord, Cell], coord: Coord): Map[Coord, Cell] = {
    val bellow = Coord(coord.x, coord.y + 1)
    val bellowLeft = Coord(coord.x - 1, coord.y + 1)
    val bellowRight = Coord(coord.x + 1, coord.y + 1)

    (map.getOrElse(bellow, Air), map.getOrElse(bellowLeft, Air), map.getOrElse(bellowRight, Air)) match {
        case (Air, _, _) | (_, Air, _) | (_, _, Air) if bellow.y == floor =>
            val mapWithFloor = map + (bellowLeft -> Rock) + (bellow -> Rock) + (bellowRight -> Rock)
            spawnSandPart2(mapWithFloor, coord)
        case (Air, _, _) => spawnSandPart2(map, bellow)
        case (_, Air, _) => spawnSandPart2(map, bellowLeft)
        case (_, _, Air) => spawnSandPart2(map, bellowRight)
        case _ => map + (coord -> Sand)
    }
}

val part2 = {
    var map: Map[Coord, Cell] = initialMap
    var count = 0

    while (map(Coord(startX, startY)) != Sand) {
        map = spawnSandPart2(map, Coord(startX, startY))
        //printMap(map)
        //if (count == 100)
        //    map = map + (Coord(startX, startY) -> Sand)
        count += 1
    }

    count
}