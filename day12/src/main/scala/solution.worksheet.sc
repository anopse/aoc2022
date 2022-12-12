import scala.util.parsing.combinator._
import scala.annotation.tailrec

val lines = scala.io.Source.fromFile("input.txt").getLines().toArray

val height = lines.length
val width = lines(0).length

val grid = lines.map(_.toCharArray).flatten

def _2dto1d(x: Int, y: Int) = y * width + x
def _1dto2d(i: Int) = (i % width, i / width)

val entry = grid.indexOf('S')
val exit = grid.indexOf('E')

def canMoveTo(from: Char, to: Char) = (from, to) match {
  case ('E', _) => false
  case ('z', 'E') => true
  case (_, 'E') => false
  case ('S', _) => true
  case (_, 'S') => false
  case (f, t) => f.toInt + 1 >= t.toInt
}

val graph: Array[List[Int]] = {
  var g: Array[List[Int]] = Array.fill(grid.length)(Nil)

  for (y <- 0 until height) {
    for (x <- 0 until width) {
      val f = _2dto1d(x, y)
      val neighbours = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
        .filter { case (x, y) => x >= 0 && x < width && y >= 0 && y < height }
        .map { case (x, y) => _2dto1d(x, y) }
        .filter(i => canMoveTo(grid(f), grid(i)))

      g(f) = neighbours 
    }
  }

  g
}

sealed trait Trampoline[A]
case class Done[A](value: A) extends Trampoline[A]
case class More[A](call: () => Trampoline[A]) extends Trampoline[A]

@tailrec
final def run[A](trampoline: Trampoline[A]): A = {
  trampoline match {
    case Done(v) => v
    case More(t) => run(t())
  }
}

def optionMin(a: Option[Int], b: Option[Int]): Option[Int] = (a, b) match {
  case (Some(x), Some(y)) => Some(x min y)
  case (Some(x), None) => Some(x)
  case (None, Some(y)) => Some(y)
  case (None, None) => None
}

def trampolineFold[A, B](seq: List[Trampoline[A]], init: B)(f: (B, A) => B): Trampoline[B] = {
    seq match {
        case Nil => Done(init)
        case x :: xs =>
            x match {
                case Done(v) => trampolineFold(xs, f(init, v))(f)
                case More(t) => More(() => trampolineFold(t() :: xs, init)(f))
            }
    }
}

val distMap: Array[Int] = Array.fill(grid.length)(Int.MaxValue)

def findShortestPath(from: Int, currentDist: Int): Trampoline[Option[Int]] = {
    if (from == exit) {
        Done(Some(currentDist))
    } else {
        val neighbours = graph(from)
        val newDist = currentDist + 1
        val visit = neighbours.filter(distMap(_) > newDist)
        for { v <- visit } distMap(v) = newDist
        //println(s"Visiting $from, neighbours: $neighbours, visit: $visit, currentDist: $currentDist")

        if (visit.isEmpty) {
            Done(None)
        } else {
            More(() => trampolineFold(visit.map(findShortestPath(_, newDist)), None: Option[Int])(optionMin))
        }
    }
}

val part1 = run(findShortestPath(entry, 0))


def findShortestPathPart2(from: Int, currentDist: Int): Trampoline[Option[Int]] = {
    if (from == exit) {
        Done(Some(currentDist))
    } else {
        val neighbours = graph(from)
        val cd = if (grid(from) == 'a') 0 else currentDist
        val newDist = cd + 1
        val visit = neighbours.filter(distMap(_) > newDist)
        for { v <- visit } distMap(v) = newDist
        //println(s"Visiting $from, neighbours: $neighbours, visit: $visit, currentDist: $currentDist")

        if (visit.isEmpty) {
            Done(None)
        } else {
            More(() => trampolineFold(visit.map(findShortestPathPart2(_, newDist)), None: Option[Int])(optionMin))
        }
    }
}

for { x <- distMap.indices } distMap(x) = Int.MaxValue

val part2 = run(findShortestPathPart2(entry, 0))