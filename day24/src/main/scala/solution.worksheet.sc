import scala.util.parsing.combinator._
import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

val debugPrint = false
val isSampleInput = false

def dprintln(o: => Any) = if (debugPrint) println(o)
def dprintln() = if (debugPrint) println()
def dprint(o: => Any) = if (debugPrint) print(o)

val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).getLines.toList

// Input format :
//  #.######
//  #>>.<^<#
//  #.<..<<#
//  #>v.><>#
//  #<^v^^>#
//  ######.#

val minX = 1
val maxX = input.head.length - 2
val minY = 1
val maxY = input.length - 2

val entranceX = 1
val entranceY = 0
val exitX = maxX
val exitY = maxY + 1

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

sealed case class Blizzard(x: Int, y: Int, dir: Direction)

val blizzards = {
for {
    (line, y) <- input.zipWithIndex
    (c, x) <- line.zipWithIndex
    dir <- c match {
        case '>' => Some(East)
        case '<' => Some(West)
        case '^' => Some(North)
        case 'v' => Some(South)
        case '#' => None
        case '.' => None
        case x => throw new Exception(s"Unexpected character $x")
    }
    } yield Blizzard(x, y, dir)
}.toList

val horizontalBlizzards = blizzards.filter(b => b.dir == East || b.dir == West)
val verticalBlizzards = blizzards.filter(b => b.dir == North || b.dir == South)

def stepBlizzard(b: Blizzard) = b.dir match {
    case East if b.x >= maxX => Blizzard(minX, b.y, East)
    case East => Blizzard(b.x + 1, b.y, East)
    case West if b.x <= minX => Blizzard(maxX, b.y, West)
    case West => Blizzard(b.x - 1, b.y, West)
    case North if b.y <= minY => Blizzard(b.x, maxY, North)
    case North => Blizzard(b.x, b.y - 1, North)
    case South if b.y >= maxY => Blizzard(b.x, minY, South)
    case South => Blizzard(b.x, b.y + 1, South)
}

def stepAllBlizzards(blizzards: List[Blizzard]) = blizzards.map(stepBlizzard)

def toInfiniteSteps[T, S](initial: T, nextStep: T => T, mapResult: T => S): LazyList[S] = {
    val initialResult = mapResult(initial)
    val steps = LazyList.iterate(initial)(nextStep).drop(1).map(mapResult).takeWhile(_ != initialResult).toList
    dprintln(steps.length + 1)
    LazyList.continually(initialResult::steps).flatten
}

type BlizzardStream = LazyList[Set[(Int, Int)]]

val infiniteHorizontalSteps: BlizzardStream = toInfiniteSteps(horizontalBlizzards, stepAllBlizzards, _.map(b => b.x -> b.y).toSet)
val infiniteVerticalSteps: BlizzardStream = toInfiniteSteps(verticalBlizzards, stepAllBlizzards, _.map(b => b.x -> b.y).toSet)

if (debugPrint) {
    println("Map")
    for {
        i <- 0 to 20
    } {
        println()
        println(i)
        println()
        for {
            y <- minY to maxY
        } {
            for {
                x <- minX to maxX
            } {
                val horizontals = infiniteHorizontalSteps(i)
                val verticals = infiniteVerticalSteps(i)
                val horizontal = horizontals.contains((x, y))
                val vertical = verticals.contains((x, y))
                if (horizontal && vertical) {
                    print('2')
                } else if (horizontal) {
                    print('>')
                } else if (vertical) {
                    print('^')
                } else {
                    print('.')
                }
            }
            println()
        }
    }
}


sealed case class State(x: Int, y: Int, steps: Int, horizontalBlizzards: BlizzardStream, verticalBlizzards: BlizzardStream)

val initialState = State(entranceX, entranceY, 0, infiniteHorizontalSteps.drop(1), infiniteVerticalSteps.drop(1))//, List.empty)

val moveOptions = Array((0, 1), (0, -1), (1, 0), (-1, 0), (0, 0))

final def manhattanDistance(x1: Int, y1: Int, x2: Int, y2: Int) = Math.abs(x1 - x2) + Math.abs(y1 - y2)

sealed case class CacheEntry(x: Int, y: Int, horizontalBlizzards: Int, verticalBlizzards: Int)

val horizontalBlizPossibilities = infiniteHorizontalSteps.take(maxX).toArray
val verticalBlizPossibilities = infiniteVerticalSteps.take(maxY).toArray

final def generateCacheEntry(state: State) = CacheEntry(state.x, state.y, horizontalBlizPossibilities.indexOf(state.horizontalBlizzards.head), verticalBlizPossibilities.indexOf(state.verticalBlizzards.head))

final def findShortestExit(startingState: State, targetX: Int, targetY: Int, limit: Int): Option[State] = {
    var toProcess = PriorityQueue(startingState)(Ordering.by(s => -manhattanDistance(s.x, s.y, targetX, targetY)))
    var cache = Map.empty[CacheEntry, Int]
    var bestFound: Option[State] = None

    while (!toProcess.isEmpty) {
        val next = toProcess.dequeue()
        if (next.steps >= bestFound.map(_.steps).getOrElse(Int.MaxValue) || next.steps >= limit) {
            // skip this one
        } else if (next.x == targetX && next.y == targetY) {
            bestFound = Some(next)
        } else {
            val cacheEntry = generateCacheEntry(next)
            if (cache.get(cacheEntry).exists(_ <= next.steps)) {
                // skip this one
            } else {
                cache += cacheEntry -> next.steps

                val currentHorBlizzs = next.horizontalBlizzards.head
                val currentVerBlizzs = next.verticalBlizzards.head
                val newHorBlizzs = next.horizontalBlizzards.tail
                val newVerBlizzs = next.verticalBlizzards.tail
                val newSteps = next.steps + 1
                
                for {
                    (dx, dy) <- moveOptions
                    newX = next.x + dx
                    newY = next.y + dy
                    isInside = newX >= minX && newX <= maxX && newY >= minY && newY <= maxY
                    isNotMoving = dx == 0 && dy == 0
                    isTarget = newX == targetX && newY == targetY
                    if isInside || isNotMoving || isTarget
                    isHittingHorBlizz = currentHorBlizzs.contains((newX, newY))
                    isHittingVerBlizz = currentVerBlizzs.contains((newX, newY))
                    if !isHittingHorBlizz && !isHittingVerBlizz
                } {
                    val newState = State(newX, newY, newSteps, newHorBlizzs, newVerBlizzs)
                    toProcess.enqueue(newState)
                }
            }
        }
    }

    bestFound    
}

println(Runtime.getRuntime().maxMemory())

//val part1ExitOpt = findShortestExit(initialState, exitX, exitY, 400)
//val part1Exit = part1ExitOpt.get
//val part1 = part1Exit.steps
//println(s"Part 1: $part1")

// Part 2
//val backToEntrance = findShortestExit(part1Exit, entranceX, entranceY, 800).get
//val frontToEntrance = findShortestExit(backToEntrance, exitX, exitY, 1200).get
//println(s"Part 2: ${frontToEntrance.steps}")