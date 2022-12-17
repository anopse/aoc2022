import scala.util.parsing.combinator._
import scala.annotation.tailrec

val isSampleInput = false
val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format:
// >>><<><>><<<>><>>

sealed trait Move
case object Left extends Move
case object Right extends Move

def parseMove(c: Char): Move = c match
  case '<' => Left
  case '>' => Right

def parseMoves(input: String): List[Move] = input.map(parseMove).toList

val moves = parseMoves(input)

val infiniteMoves = LazyList.continually(moves).flatten

case class Shape(lines: List[Byte]) {
    def apply(move: Move): Shape = {
        move match {
            case Right if lines.forall(b => (b & 1) == 0) => Shape(lines.map(b => (b >> 1).toByte))
            case Left if lines.forall(b => (b & (1 << 6)) == 0) => Shape(lines.map(b => (b << 1).toByte))
            case _ => this
        }
    }
}

// ####.|
val horizontalLineShape = Shape(List(
    Integer.parseInt("11110", 2).toByte
))

// .#...|
// ###..|
// .#...|
val plusShape = Shape(List(
    Integer.parseInt("01000", 2).toByte,
    Integer.parseInt("11100", 2).toByte,
    Integer.parseInt("01000", 2).toByte
))

// ..#..|
// ..#..|
// ###..|
val lShape = Shape(List(
    Integer.parseInt("00100", 2).toByte,
    Integer.parseInt("00100", 2).toByte,
    Integer.parseInt("11100", 2).toByte
))

// #....|
// #....|
// #....|
// #....|
val verticalLineShape = Shape(List(
    Integer.parseInt("10000", 2).toByte,
    Integer.parseInt("10000", 2).toByte,
    Integer.parseInt("10000", 2).toByte,
    Integer.parseInt("10000", 2).toByte
))

// ##...|
// ##...|
val squareShape = Shape(List(
    Integer.parseInt("11000", 2).toByte,
    Integer.parseInt("11000", 2).toByte
))

val shapes = List(horizontalLineShape, plusShape, lShape, verticalLineShape, squareShape)

val infiniteShapes = LazyList.continually(shapes).flatten

case class State(
    nextShapes: LazyList[Shape],
    nextMoves: LazyList[Move],
    lines: Array[Byte]
)

def ensureEnoughSpace(lines: Array[Byte], shape: Shape): Array[Byte] = {
    val freeSpace = lines.reverseIterator.takeWhile(_ == 0).length
    val requiredFreeSpace = 3 + shape.lines.length
    //println(s"Free space: $freeSpace, required: $requiredFreeSpace")

    if freeSpace < requiredFreeSpace then {
        val newLines = lines ++ Array.fill[Byte](requiredFreeSpace - freeSpace)(0)
        newLines
    } else if freeSpace > requiredFreeSpace then {
        val newLines = lines.dropRight(freeSpace - requiredFreeSpace)
        newLines
    } else {
        lines
    }
}

@tailrec
final def isColliding(lines: Array[Byte], shapeLines: List[Byte], yPos: Int): Boolean = {
    shapeLines match {
        case l::_ if yPos == -1 => true
        case Nil => false
        case l :: rest =>
            val line = lines(yPos)
            val collision = (line & l) != 0
            if collision then true
            else isColliding(lines, rest, yPos - 1)
    }
}

@tailrec
final def fuse(lines: Array[Byte], shapeLines: List[Byte], yPos: Int): Array[Byte] = {
    shapeLines match {
        case Nil => lines
        case l :: rest =>
            val line = lines(yPos)
            val fusedLine = line | l
            lines(yPos) = fusedLine.toByte
            fuse(lines, rest, yPos - 1)
    }
}

def lineToString(b: Byte): String = {
    val s = Integer.toBinaryString(b)
    val padded = s.reverse.padTo(7, '0').reverse
    val l = padded.replace('0', '.').replace('1', '#')
    s"|$l|"
}

def printStepping(lines: Array[Byte], shape: Shape, yPos: Int) = {
    val fused = fuse(lines.clone(), shape.lines, yPos)
    println()
    for {
        y <- fused.length - 1 to 0 by -1
        line = fused(y)
    } {
        println(lineToString(line))
    }
}

def step(state: State): State = {
    val newNextShapes = state.nextShapes.tail
    var shape = state.nextShapes.head
    val lines = ensureEnoughSpace(state.lines, shape)
    var newNextMoves = state.nextMoves

    //print("A new rock begins falling")

    var yPos = lines.length - 1
    while (!isColliding(lines, shape.lines, yPos)) {
        //printStepping(lines, shape, yPos)
        val move = newNextMoves.head
        newNextMoves = newNextMoves.tail
        //println()
        //print(s"Jet of gas pushes rock $move")
        
        val shapeAfterMove = shape(move)
        if (shapeAfterMove != shape && !isColliding(lines, shapeAfterMove.lines, yPos)) {
            shape = shapeAfterMove
            //println()
        } else {
            //println(" but nothing happens")
        }
        //printStepping(lines, shape, yPos)

        //print("Rock falls 1 unit")
        yPos -= 1
    }

    //println(" causing it to come to rest")
    yPos += 1
    //printStepping(lines, shape, yPos)
    val newLines = fuse(lines.clone(), shape.lines, yPos)

    State(newNextShapes, newNextMoves, newLines)
}

def printState(state: State) = {
    for {
        y <- state.lines.length - 1 to 0 by -1
        line = state.lines(y)
    } {
        println(lineToString(line))
    }
}


def mesureHeight(state: State): Int = {
    state.lines.takeWhile(_ != 0).length
}

val initialState = State(infiniteShapes, infiniteMoves, Array.empty)


var part1State = initialState

for {
    i <- 1 to 2022
} {
    part1State = step(part1State)
}

val part1Height = mesureHeight(part1State)

// part 2


def simplifyState(state: State): (State, Int) = {
    var prevLine = 0
    var found = false
    val targetMask = 64 | 32 | 16 | 8 | 4 | 2 | 1
    var lineRetained = 0
    val lines = state.lines

    lines.reverseIterator.takeWhile(l => {
        val mask = l | prevLine
        found = mask == targetMask
        prevLine = l
        lineRetained += 1
        !found
    }).size

    if (!found) {
        (state, 0)
    } else {
        lineRetained = lineRetained - 1
        val linesRemoved = lines.length - lineRetained
        //println(s"Removed $linesRemoved lines, $lineRetained lines retained, out of ${lines.length}")
        val newLines = lines.drop(linesRemoved)
        (state.copy(lines = newLines), linesRemoved)
    }
}


var part2State = initialState
var removedLines = 0L

for {
    i <- 1L to 20000L
} {
    part2State = step(part2State)
    val (simplifiedState, linesRemoved) = simplifyState(part2State)
    removedLines += linesRemoved
    if (linesRemoved > 0) { 
        println(s"Removed $linesRemoved lines at step $i")
        println(s"Height: ${mesureHeight(simplifiedState) + removedLines}")
    }
    if (i > 18000) {
        println(s"Step $i, height: ${mesureHeight(simplifiedState) + removedLines}")
    }
    if (i % 1000 == 0) println(s"Step $i, height: ${mesureHeight(simplifiedState) + removedLines}")
    part2State = simplifiedState
}

val part2Height = mesureHeight(part2State) + removedLines

// part 2 solved by detecting repeating pattern of removed lines :
//  A cycle of length 1730, each cycle adds up 2659 heights
//   for example at step 18210 and step 19940
// between 19940 and 999_999_999_040 there's 578_034_670 cycles
// we need 960 more steps, 960 more steps ahead of our pattern we add height 1452
// so : 19940 height is 30687 => 999_999_999_040 height is 1_536_994_218_217
// 999_999_999_040 + 960 cycles = 1,000,000,000,000
// height 1_536_994_218_217 + 1_452 = 1_536_994_219_669