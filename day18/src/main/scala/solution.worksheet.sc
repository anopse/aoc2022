import scala.util.parsing.combinator._
import scala.annotation.tailrec

val isSampleInput = false
val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).getLines()

// Input format :
// 8,16,18
// 14,19,12
// ...

sealed case class Coord(x: Int, y: Int, z: Int)

final def parseCoord(s: String): Coord =
  val Array(x, y, z) = s.split(",").map(_.toInt)
  Coord(x, y, z)

val coords = input.map(parseCoord).toArray

// 0 >= x >= 20
// 0 >= y >= 20
// 0 >= z >= 21

val countX = coords.map(_.x).max + 1
val countY = coords.map(_.y).max + 1
val countZ = coords.map(_.z).max + 1

final def xyToIndex(x: Int, y: Int): Int = x * countY + y
final def zToBit(z: Int): Int = 1 << z

val grid = Array.fill(countX * countY)(0)
for {
    Coord(x, y, z) <- coords
    index = xyToIndex(x, y)
    bit = zToBit(z)
} {
    grid(index) |= bit
}

final def sides(c: Coord): Array[Coord] = {
    val Coord(x, y, z) = c

    Array(
        Coord(x - 1, y, z),
        Coord(x + 1, y, z),
        Coord(x, y - 1, z),
        Coord(x, y + 1, z),
        Coord(x, y, z - 1),
        Coord(x, y, z + 1),
    )
}

final def isOutsideRange(c: Coord): Boolean = {
    val Coord(x, y, z) = c
    x < 0 || x >= countX || y < 0 || y >= countY || z < 0 || z >= countZ
}

final def isPresent(c: Coord): Boolean = {
    val Coord(x, y, z) = c
    if (isOutsideRange(c)) then
        false
    else {
        val index = xyToIndex(x, y)
        val bit = zToBit(z)

        (grid(index) & bit) != 0
    }
}

final def countNeighbours(c: Coord): Int = sides(c).count(isPresent)
final def countOpenFaces(c: Coord): Int = sides(c).count(!isPresent(_))

val part1 = coords.map(countOpenFaces).sum

val outsideGrid = Array.fill(countX * countY)(0)

final def flagOutside(c: Coord): Unit = {
    val Coord(x, y, z) = c
    val index = xyToIndex(x, y)
    val bit = zToBit(z)

    //println(s"Flagging $c")

    outsideGrid(index) |= bit
}

final def isFlaggedOutside(c: Coord): Boolean = {
    val Coord(x, y, z) = c
    if (isOutsideRange(c)) then
        true
    else {
        val index = xyToIndex(x, y)
        val bit = zToBit(z)

        (outsideGrid(index) & bit) != 0
    }
}

@tailrec
final def floodFill(toVisit: List[Coord]): Unit = {
    toVisit match {
        case Nil => ()
        case c :: rest =>
            if (isOutsideRange(c) || isPresent(c) || isFlaggedOutside(c)) {
                floodFill(rest)
            } else {
                flagOutside(c)
                floodFill(sides(c).toList ++ rest)
            }
    }
}

for {
    x <- -1 to countX
    y <- -1 to countY
    z <- -1 to countZ
    c = Coord(x, y, z)
    if isOutsideRange(c)
    n <- sides(c)
} {
    floodFill(List(n))
}

val part2 = coords.view.flatMap(sides).count(isFlaggedOutside)

// for {
//     x <- -1 to countX
//     y <- -1 to countY
//     z <- -1 to countZ
//     c = Coord(x, y, z)
// } {
//     if (!isFlaggedOutside(c) && !isPresent(c)) {
//         println(s"Unflagged $c")
//     }
// }