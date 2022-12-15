import scala.util.parsing.combinator._
import scala.annotation.tailrec

val isSampleInput = false
val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format:
// Sensor at x=2, y=18: closest beacon is at x=-2, y=15
// Sensor at x=9, y=16: closest beacon is at x=10, y=16

case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    def -(other: Coord): Coord = Coord(x - other.x, y - other.y)
    def ==(other: Coord): Boolean = x == other.x && y == other.y
    def !=(other: Coord): Boolean = x != other.x || y != other.y
    def <(other: Coord): Boolean = x < other.x && y < other.y
    def >(other: Coord): Boolean = x > other.x && y > other.y
    def <=(other: Coord): Boolean = x <= other.x && y <= other.y
    def >=(other: Coord): Boolean = x >= other.x && y >= other.y
    def manhattanDistance(other: Coord): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
    def abs: Coord = Coord(Math.abs(x), Math.abs(y))
    override def toString: String = s"($x;$y)"
}

case class Sensor(position: Coord, beacon: Coord) {
    override def toString(): String = s"S$position->B$beacon"
}

// separated by a blank line
type Problem = List[Sensor]

object Parser extends RegexParsers with PackratParsers {
    val number: PackratParser[Int] = """-?\d+""".r ^^ { _.toInt }
    val coord: PackratParser[Coord] = "x=" ~ number ~ "," ~ "y=" ~ number ^^ { case  "x=" ~ x ~ "," ~ "y=" ~ y => Coord(x, y) }
    val sensor: PackratParser[Sensor] = "Sensor at " ~ coord ~ ": closest beacon is at " ~ coord ^^ {
         case "Sensor at " ~ position ~ ": closest beacon is at " ~ beacon => Sensor(position, beacon) 
    }
    val problem: PackratParser[Problem] = repsep(sensor, "")
}

val rawProblem = Parser.parseAll(Parser.problem, input)
val problem = rawProblem.get

val knownBeacons = problem.map(_.beacon).toSet

case class Range(center: Coord, radius: Int)

val noBeaconsRanges = problem.map { sensor =>
    val beaconRadius = sensor.position.manhattanDistance(sensor.beacon)

    Range(sensor.position, beaconRadius)
}

case class Range1D(min: Int, max: Int) {
    def contains(x: Int): Boolean = x >= min && x <= max
    def size: Int = max - min + 1
    def intersects(other: Range1D): Boolean = {
        contains(other.min) || contains(other.max) || other.contains(min) || other.contains(max)
    }
    def iterator: Iterator[Int] = (min to max).iterator
}

def morphRangeToY(range: Range, y: Int): Option[Range1D] = {
    val dist = range.center.manhattanDistance(Coord(range.center.x, y))
    val newRadius = range.radius - dist

    if (newRadius <= 0)
        None
    else
        Some(Range1D(range.center.x - newRadius, range.center.x + newRadius))
}

val targetY = if isSampleInput then 10 else 2000000

val noBeaconsRangesOnTargetY = noBeaconsRanges.flatMap(morphRangeToY(_, targetY))

def fuseTwoRange(r1: Range1D, r2: Range1D): Option[Range1D] = {
    if (r1.contains(r2.min) || r2.contains(r1.min)) {
        val newMin = Math.min(r1.min, r2.min)
        val newMax = Math.max(r1.max, r2.max)
        val newRange = Range1D(newMin, newMax)
        Some(newRange)
    } else {
        None
    }
}

def fuseRanges(ranges: List[Range1D]): List[Range1D] = {
    ranges match {
        case Nil => Nil
        case r :: Nil => List(r)
        case r :: rest => {
            val r2 = rest.find(r2 => fuseTwoRange(r, r2).isDefined)
            r2 match {
                case None => r :: fuseRanges(rest)
                case Some(r2) => {
                    val newRange = fuseTwoRange(r, r2).get
                    val newRest = rest.filter(_ != r2)
                    fuseRanges(newRange :: newRest)
                }
            }
        }
    }
}

val fusedRanges = fuseRanges(noBeaconsRangesOnTargetY)
val part1 = fusedRanges.map(_.size).sum - 1

// part 2

val minX = 0
val maxX = if (isSampleInput) 20 else 4000000
val minY = 0
val maxY = if (isSampleInput) 20 else 4000000

def substractRange(outer: Range1D, inner: Range1D): List[Range1D] = {
    if (!outer.intersects(inner)) {
        List(outer)
    } else {
        val isOuterOnLeft = outer.min <= inner.min
        val isOuterOnRight = outer.max >= inner.max

        val r = if (isOuterOnLeft && isOuterOnRight) {
            List(Range1D(outer.min, inner.min-1), Range1D(inner.max+1, outer.max))
        } else if (isOuterOnLeft) {
            List(Range1D(outer.min, inner.min-1))
        } else if (isOuterOnRight) {
            List(Range1D(inner.max+1, outer.max))
        } else {
            Nil
        }

        r.filter(_.size > 0)
    }
}

val test = substractRange(Range1D(0, 10), Range1D(1, 10))

var beaconLocations = List.empty[Coord]

for {
    y <- minY to maxY
} {
    var possibilities = List(Range1D(minX, maxX))
    val noBeaconRangesAtY = noBeaconsRanges.flatMap(morphRangeToY(_, y))
    for {
        nbr <- noBeaconRangesAtY
    } {
        possibilities = possibilities.flatMap(substractRange(_, nbr))
    }
    val remains = possibilities.flatMap(_.iterator).toSet
    for {
        x <- remains
    } {
        beaconLocations = Coord(x, y) :: beaconLocations
    }
}

val found = beaconLocations
val foundX = found.head.x
val foundY = found.head.y
val part2 = foundX * 4000000L + foundY