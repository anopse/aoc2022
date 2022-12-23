import scala.util.parsing.combinator._
import scala.annotation.tailrec

val debugPrint = true
val isSampleInput = false

def dprintln(o: => Any) = if (debugPrint) println(o)
def dprintln() = if (debugPrint) println()
def dprint(o: => Any) = if (debugPrint) print(o)

val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).getLines.toList

// Input format :
//  ....#
//  ..#..
//  #...#

sealed case class Coord(x: Int, y: Int)

val initialElves = for {
    (line, y) <- input.zipWithIndex
    (c, x) <- line.zipWithIndex
    if c == '#'
} yield Coord(x, y)

sealed case class State(elves: Set[Coord], instructions: LazyList[Instruction])

sealed trait CardinalDirection {
    def apply(c: Coord): Coord
}
case object North extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x, c.y - 1)
}
case object South extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x, c.y + 1)
}
case object East extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x + 1, c.y)
}
case object West extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x - 1, c.y)
}
case object NorthEast extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x + 1, c.y - 1)
}
case object NorthWest extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x - 1, c.y - 1)
}
case object SouthEast extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x + 1, c.y + 1)
}
case object SouthWest extends CardinalDirection {
    def apply(c: Coord) = Coord(c.x - 1, c.y + 1)
}

val allDirections = List(North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest)

final def getNeighbours(c: Coord): List[Coord] = allDirections.map(_(c))

final def neighbours(state: State, c: Coord): List[Boolean] = getNeighbours(c).map(state.elves.contains)

final def countNeighbours(state: State, c: Coord): Int = neighbours(state, c).count(identity)

final def shouldSleep(state: State, c: Coord): Boolean = {
    val n = countNeighbours(state, c)

    n == 0
}

sealed trait Instruction {
    def intent(state: State, c: Coord): Option[Coord]
}

case object WantNorth extends Instruction {
    def intent(state: State, c: Coord): Option[Coord] = {
        val dest = North(c)
        val destN1 = NorthEast(c)
        val destN2 = NorthWest(c)

        if state.elves.contains(dest) then None
        else if state.elves.contains(destN1) then None
        else if state.elves.contains(destN2) then None
        else Some(dest)
    }
}
case object WantSouth extends Instruction {
    def intent(state: State, c: Coord): Option[Coord] = {
        val dest = South(c)
        val destN1 = SouthEast(c)
        val destN2 = SouthWest(c)

        if state.elves.contains(dest) then None
        else if state.elves.contains(destN1) then None
        else if state.elves.contains(destN2) then None
        else Some(dest)
    }
}
case object WantEast extends Instruction {
    def intent(state: State, c: Coord): Option[Coord] = {
        val dest = East(c)
        val destN1 = NorthEast(c)
        val destN2 = SouthEast(c)

        if state.elves.contains(dest) then None
        else if state.elves.contains(destN1) then None
        else if state.elves.contains(destN2) then None
        else Some(dest)
    }
}
case object WantWest extends Instruction {
    def intent(state: State, c: Coord): Option[Coord] = {
        val dest = West(c)
        val destN1 = NorthWest(c)
        val destN2 = SouthWest(c)

        if state.elves.contains(dest) then None
        else if state.elves.contains(destN1) then None
        else if state.elves.contains(destN2) then None
        else Some(dest)
    }
}

val allInstructions = List(WantNorth, WantSouth, WantWest, WantEast)
val infiniteInstructions = LazyList.continually(allInstructions).flatten

final def getIntents(state: State, instructions: List[Instruction]): Map[Coord, Coord] = {
    state.elves
        .view
        .filterNot { case c => shouldSleep(state, c) }
        .map(elf => {
                val intent = instructions
                    .map(_.intent(state, elf))
                    .collectFirst { case Some(dest) => dest }

                elf -> intent
            })
        .collect { case (c, Some(dest)) => (c, dest) }
        .toMap
}

final def removeConflictingIntents(intents: Map[Coord, Coord]): Map[Coord, Coord] = {
    val conflictingIntents = for {
        (c1, dest1) <- intents
        (c2, dest2) <- intents
        if c1 != c2 && dest1 == dest2
    } yield (c1, c2)

    val conflictingCoords = conflictingIntents.flatMap { case (c1, c2) => List(c1, c2) }.toSet

    intents.filter { case (c, _) => !conflictingCoords.contains(c) }
}

final def moveElves(state: State, intents: Map[Coord, Coord]): Set[Coord] = {
    val elvesToRemove = intents.keys
    val newElves = intents.values

    (state.elves -- elvesToRemove) ++ newElves
}

final def step(state: State): State = {
    val currentInstructions = state.instructions.take(allInstructions.length).toList
    val newInstructions = state.instructions.drop(allInstructions.length + 1)
    val intent = getIntents(state, currentInstructions)
    val removedConflicts = removeConflictingIntents(intent)
    val afterMoveElves = moveElves(state, removedConflicts)

    State(afterMoveElves, newInstructions)
}

@tailrec
final def run(state: State, n: Int): State = {
    if n == 0 then state
    else run(step(state), n - 1)
}

val initialState = State(initialElves.toSet, infiniteInstructions)

final def dprintState(state: State) = {
    if (debugPrint) {
        val minX = state.elves.map(_.x).min
        val maxX = state.elves.map(_.x).max
        val minY = state.elves.map(_.y).min
        val maxY = state.elves.map(_.y).max

        for {
            y <- minY to maxY
        } {
            for {
                x <- minX to maxX
            } {
                val c = Coord(x, y)
                val e = state.elves.contains(c)
                e match {
                    case true => print("#")
                    case false => print(".")
                }
            }
            println()

        }
        println()
        println(s"next instruction: ${state.instructions.head}")
    }
}

final def score(state: State): Int = {
    val minX = state.elves.map(_.x).min
    val maxX = state.elves.map(_.x).max
    val minY = state.elves.map(_.y).min
    val maxY = state.elves.map(_.y).max

    val emptySpaces = for {
        y <- minY to maxY
        x <- minX to maxX
        c = Coord(x, y)
        if !state.elves.contains(c)
    } yield 1

    emptySpaces.sum
}

val part1State = run(initialState, 10)
val part1Score = score(part1State)

final def isDone(state: State): Boolean = {
    state.elves.forall(c => shouldSleep(state, c))
}

@tailrec
final def runUntilDone(state: State, acc: Int): (State, Int) = {
    if isDone(state) then (state, acc)
    else runUntilDone(step(state), acc + 1)
}

val (part2State, part2Score) = runUntilDone(initialState, 1)
println(s"part 1: $part1Score")
println(s"part 2: $part2Score")
