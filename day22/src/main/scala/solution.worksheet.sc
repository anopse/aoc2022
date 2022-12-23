import scala.util.parsing.combinator._
import scala.annotation.tailrec

val debugPrint = true
val isSampleInput = true

def dprintln(o: => Any) = if (debugPrint) println(o)
def dprintln() = if (debugPrint) println()
def dprint(o: => Any) = if (debugPrint) print(o)

val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format :
//     ...#...
//  ..#...#...
//
// 10R5L5R10L4R5L5

val parts = input.split("\r\n\r\n")
val upperPart = parts(0).linesIterator.toArray.map(_.toArray)
val lowerPart = parts(1)

val minX = 0
val maxX = upperPart.map(_.length).max
val minY = 0
val maxY = upperPart.length

sealed trait Cell
case class Empty(c: Coord) extends Cell
case object Void extends Cell
case object Wall extends Cell

sealed case class Coord(x: Int, y: Int)

val terrain = {
    var m: Map[Coord, Cell] = Map.empty
    for y <- minY until maxY do
        for x <- minX until maxX do
            val chr = upperPart.lift(y).flatMap(_.lift(x))
            val cell = chr match
                case None => Void
                case Some(' ') => Void
                case Some('#') => Wall
                case Some('.') => Empty(Coord(x, y))
                case Some(c) => throw new Exception(s"Unknown cell type: $c")
            m = m + (Coord(x, y) -> cell)
    m
}

sealed trait Instruction
sealed case class Move(n: Int) extends Instruction
sealed trait TurnInstruction extends Instruction
case object TurnLeft extends TurnInstruction
case object TurnRight extends TurnInstruction

object Parser extends RegexParsers with PackratParsers {
    val number: Parser[Int] = """\d+""".r ^^ (_.toInt)
    val move: Parser[Move] = number ^^ Move.apply
    val turnLeft: Parser[TurnLeft.type] = "L" ^^ (_ => TurnLeft)
    val turnRight: Parser[TurnRight.type] = "R" ^^ (_ => TurnRight)
    val instruction: Parser[Instruction] = move | turnLeft | turnRight
    val instructions: Parser[List[Instruction]] = instruction.+
}

val instructions = Parser.parseAll(Parser.instructions, lowerPart).get

sealed trait Facing
case object North extends Facing
case object East extends Facing
case object South extends Facing
case object West extends Facing

val allFacings = List(North, East, South, West)

sealed case class State(coord: Coord, facing: Facing)

final def turnMapping(facing: Facing, turnInstruction: TurnInstruction): Facing = {
     (facing -> turnInstruction) match {
        case (North, TurnLeft) => West
        case (North, TurnRight) => East
        case (East, TurnLeft) => North
        case (East, TurnRight) => South
        case (South, TurnLeft) => East
        case (South, TurnRight) => West
        case (West, TurnLeft) => South
        case (West, TurnRight) => North
    }
}

final def turn(state: State, turnInstruction: TurnInstruction): State = {
    val newFacing = turnMapping(state.facing, turnInstruction)

    State(state.coord, newFacing)
}

val initialCoord = {
    (minY until maxY).flatMap(y =>
        (minX until maxX).flatMap(x =>
            terrain(Coord(x, y)) match {
                case Empty(_) => Some(Coord(x, y))
                case _ => None
            }
        )
    )
}.head

val initialFacing: Facing = East

val initialState = State(initialCoord, initialFacing)

final def goingTo(coord: Coord, facing: Facing): LazyList[Coord] = {
    val nextCoord = facing match {
        case North if coord.y == minY => Coord(coord.x, maxY)
        case North => Coord(coord.x, coord.y - 1)
        case East if coord.x == maxX => Coord(minX, coord.y)
        case East => Coord(coord.x + 1, coord.y)
        case South if coord.y == maxY => Coord(coord.x, minY)
        case South => Coord(coord.x, coord.y + 1)
        case West if coord.x == minX => Coord(maxX, coord.y)
        case West => Coord(coord.x - 1, coord.y)
    }

    nextCoord #:: goingTo(nextCoord, facing)
}


def move(state: State, distance: Int): State = {
    val facing = state.facing
    val coord = state.coord

    goingTo(coord, facing)
        .filter(c => terrain.get(c).filter(_ != Void).isDefined)
        .take(distance)
        .takeWhile(c => terrain(c) != Wall)
        .lastOption match {
        case None => state
        case Some(c) => State(c, facing)
    }
}

def printState(state: State): Unit = {
    if (debugPrint) {
        println()
        println()
        for {
            y <- minY until maxY
        } {
            for {
                x <- minX until maxX
            } {
                if (state.coord == Coord(x, y)) {
                    state.facing match {
                        case North => print('^')
                        case East => print('>')
                        case South => print('v')
                        case West => print('<')
                    }
                } else {
                    val chr = terrain(Coord(x, y)) match {
                        case Empty(_) => '.'
                        case Void => ' '
                        case Wall => '#'
                    }
                    print(chr)
                }
            }
            println()
        }
        println()
        println(state)
        println()
    }
}

def runInstruction(state: State, instruction: Instruction): State = {
    printState(state)
    dprintln(s"Running instruction: $instruction")
    instruction match {
        case Move(n) => move(state, n)
        case TurnLeft => turn(state, TurnLeft)
        case TurnRight => turn(state, TurnRight)
    }
}

val finalState = instructions.foldLeft(initialState)(runInstruction)

def score(state: State): Int = {
    val facingScore = state.facing match {
        case East => 0
        case South => 1
        case West => 2
        case North => 3
    }

    val coordScore = (state.coord.y + 1) * 1000 + (state.coord.x + 1) * 4

    coordScore + facingScore
}

val part1 = score(finalState)

// part 2

sealed trait DiceFace
case object Up extends DiceFace
case object Down extends DiceFace
case object Left extends DiceFace
case object Right extends DiceFace
case object Front extends DiceFace
case object Back extends DiceFace

val allDiceFaces = List(Up, Down, Left, Right, Front, Back)

sealed trait FaceTransformation
case object Clockwise extends FaceTransformation
case object CounterClockwise extends FaceTransformation

case class FaceInstance(face: DiceFace, transformations: List[FaceTransformation]) {
    def addTransformation(transformation: FaceTransformation): FaceInstance = {
        FaceInstance(face, transformation :: transformations)
    }
}

final def simpleDiceFace(face: DiceFace, going: Facing): FaceInstance = {
    (face -> going) match {
        case (Front, North) => FaceInstance(Up, Nil)
        case (Front, East) => FaceInstance(Right, Nil)
        case (Front, South) => FaceInstance(Down, Nil)
        case (Front, West) => FaceInstance(Left, Nil)

        case (Back, North) => FaceInstance(Up, List(Clockwise, Clockwise))
        case (Back, East) => FaceInstance(Left, Nil)
        case (Back, South) => FaceInstance(Down, List(Clockwise, Clockwise))
        case (Back, West) => FaceInstance(Right, Nil)

        case (Up, North) => FaceInstance(Back, List(Clockwise, Clockwise))
        case (Up, East) => FaceInstance(Left, List(Clockwise))
        case (Up, South) => FaceInstance(Front, Nil)
        case (Up, West) => FaceInstance(Right, List(CounterClockwise))

        case (Down, North) => FaceInstance(Front, Nil)
        case (Down, East) => FaceInstance(Right, List(CounterClockwise))
        case (Down, South) => FaceInstance(Back, List(Clockwise, Clockwise))
        case (Down, West) => FaceInstance(Left, List(Clockwise))

        case (Left, North) => FaceInstance(Up, List(Clockwise))
        case (Left, East) => FaceInstance(Front, Nil)
        case (Left, South) => FaceInstance(Down, List(Clockwise))
        case (Left, West) => FaceInstance(Back, Nil)

        case (Right, North) => FaceInstance(Up, List(CounterClockwise))
        case (Right, East) => FaceInstance(Back, Nil)
        case (Right, South) => FaceInstance(Down, List(Clockwise))
        case (Right, West) => FaceInstance(Front, Nil)
    }
}

final def mapDiceFace(faceInstance: FaceInstance, going: Facing): FaceInstance = {
    val FaceInstance(face, transformations) = faceInstance
    transformations match {
        case Nil => simpleDiceFace(face, going)
        case Clockwise :: rest =>
            val newGoing = going match {
                case North => East
                case East => South
                case South => West
                case West => North
            }

            mapDiceFace(FaceInstance(face, rest), newGoing).addTransformation(Clockwise)

        case CounterClockwise :: rest =>
            val newGoing = going match {
                case North => West
                case East => North
                case South => East
                case West => South
            }

            mapDiceFace(FaceInstance(face, rest), newGoing).addTransformation(CounterClockwise)
    }
}

final def simplifyTransformations(transformations: List[FaceTransformation]): List[FaceTransformation] = {
    transformations match {
        case Nil => Nil
        case Clockwise :: CounterClockwise :: rest => simplifyTransformations(rest)
        case CounterClockwise :: Clockwise :: rest => simplifyTransformations(rest)
        case CounterClockwise :: CounterClockwise :: CounterClockwise :: rest => simplifyTransformations(Clockwise :: rest)
        case Clockwise :: Clockwise :: Clockwise :: rest => simplifyTransformations(CounterClockwise :: rest)
        case head :: next => head :: simplifyTransformations(next)
    }
}

final def simplify(faceInstance: FaceInstance): FaceInstance = {
    val FaceInstance(face, transformations) = faceInstance
    var oldTransformations = transformations
    var newTransformations = simplifyTransformations(transformations)
    while (newTransformations != oldTransformations) {
        oldTransformations = newTransformations
        newTransformations = simplifyTransformations(transformations)
    }
    FaceInstance(face, simplifyTransformations(transformations))
}

val diceSize = if (isSampleInput) 4 else 50

val faceGrid = {
    val faces = for {
        x <- 0 until (maxX / diceSize)
        y <- 0 until (maxY / diceSize)
    } yield {
        val minX = diceSize * x
        val maxX = diceSize * (x + 1)
        val minY = diceSize * y
        val maxY = diceSize * (y + 1)
        val first = terrain.get(Coord(minX, minY))

        if (first == Some(Void) || first == None) {
            Coord(x, y) -> None
        } else {
            var face = Map.empty[Coord, Cell]

            for {
                x <- minX until maxX
                y <- minY until maxY
            } {
                val coord = Coord(x, y)
                val c = terrain(coord)
                val faceCoord = Coord(x - minX, y - minY)
                face = face + (faceCoord -> c)
            }

            Coord(x, y) -> Some(face)
        }
    }
    faces.toMap.view.filter(_._2.isDefined).mapValues(_.get).toMap
}

val frontFaceCoord = faceGrid.minBy(_._1.y)._1

type Faces = Map[DiceFace, Map[Coord, Cell]]

@tailrec
final def transformFace(grid: Map[Coord, Cell], transformation: List[FaceTransformation]): Map[Coord, Cell] = {
    val maxX = grid.map(_._1.x).max
    val maxY = grid.map(_._1.y).max

    transformation match {
        case Nil => grid
        case Clockwise :: rest =>
            val newGrid = for {
                (coord, cell) <- grid
            } yield {
                val newCoord = Coord(maxY - coord.y, coord.x)
                newCoord -> cell
            }
            transformFace(newGrid.toMap, rest)
        case CounterClockwise :: rest =>
            val newGrid = for {
                (coord, cell) <- grid
            } yield {
                val newCoord = Coord(coord.y, maxX - coord.x)
                newCoord -> cell
            }
            transformFace(newGrid.toMap, rest)
    }
}

final def registerFace(currentFace: FaceInstance, faceCoord: Coord, acc: Faces): Option[Faces] = {
    val FaceInstance(face, transformations) = currentFace

    if (acc.contains(face)) {
        return None
    } else {
        Some(acc + (face -> transformFace(faceGrid(faceCoord), transformations)))
    }
}

final def moveFacing(coord: Coord, facing: Facing): Option[Coord] = {
    facing match {
        case North => Some(Coord(coord.x, coord.y - 1))
        case East => Some(Coord(coord.x + 1, coord.y))
        case South => Some(Coord(coord.x, coord.y + 1))
        case West => Some(Coord(coord.x - 1, coord.y))
    }
}

final def findFaces(currentFaceCoord: Coord, currentFace: FaceInstance, acc: Faces): Faces = {
    var mutAcc = acc
    
    allFacings.foreach(facing => {
        moveFacing(currentFaceCoord, facing).filter(faceGrid.isDefinedAt).foreach(newCoord => {
            val newFace = mapDiceFace(currentFace, facing)
            val newFaceSimplified = simplify(newFace)
            dprintln(s"new face: $newFace")
            dprintln(s"raw: ${newFace}\nat coord: $newCoord")

            registerFace(newFaceSimplified, newCoord, mutAcc) match {
                case None => {}
                case Some(newAcc) =>
                    mutAcc = newAcc
                    mutAcc = findFaces(newCoord, newFaceSimplified, mutAcc)
            }
        })
    })

    mutAcc
}

val frontFace = FaceInstance(Front, Nil)
val faces = findFaces(frontFaceCoord, FaceInstance(Front, Nil), Map(Front -> faceGrid(frontFaceCoord)))

def dprintFace(face: Map[Coord, Cell]) = {
    if (debugPrint) {
        val maxX = face.map(_._1.x).max
        val maxY = face.map(_._1.y).max

        for {
            y <- 0 to maxY
        } {
            for {
                x <- 0 to maxX
            } {
                val coord = Coord(x, y)
                val cell = face(coord)
                cell match {
                    case Void => print(" ")
                    case Empty(_) => print(".")
                    case Wall => print("#")
                }
            }
            println()
        }
    }
}

for {
    (face, grid) <- faces
} {
    dprintln(face)
    dprintFace(grid)
    dprintln()
}

case class StateP2(face: DiceFace, going: Facing, coord: Coord)

val initialP2 = StateP2(Front, East, Coord(0, 0))

final def untilFaceEnd(coord: Coord, facing: Facing): LazyList[Coord] = {
    val nextCoord = facing match {
        case North if coord.y == 0 => None
        case East if coord.x == diceSize => None
        case South if coord.y == diceSize => None
        case West if coord.x == 0 => None

        case North => Some(Coord(coord.x, coord.y - 1))
        case East => Some(Coord(coord.x + 1, coord.y))
        case South => Some(Coord(coord.x, coord.y + 1))
        case West => Some(Coord(coord.x - 1, coord.y))
    }

    nextCoord.map(coord => coord #:: untilFaceEnd(coord, facing)).getOrElse(LazyList.empty)
}

final def convertCoordinates(coord: Coord, from: DiceFace, to: DiceFace): Coord = {
    (from -> to) match {
        case (Front, Front) => throw new IllegalArgumentException("Cannot convert coordinates from front to front")
        case (Front, Right) => Coord(0, coord.y)
        case (Front, Left) => Coord(diceSize - 1, coord.y)
        case (Front, Back) => throw new IllegalArgumentException("Cannot convert coordinates from front to back")
        case (Front, Up) => Coord(coord.x, 0)
        case (Front, Down) => Coord(coord.x, diceSize - 1)

        case (Right, Front) => Coord(coord.y, diceSize - 1)
        case (Right, Right) => throw new IllegalArgumentException("Cannot convert coordinates from right to right")
        case (Right, Left) => throw new IllegalArgumentException("Cannot convert coordinates from right to left")
        case (Right, Back) => Coord(coord.y, 0)
        case (Right, Up) => Coord(diceSize - 1, coord.x)
        case (Right, Down) => Coord(0, coord.x)

        case (Left, Front) => Coord(diceSize - 1 - coord.y, 0)
        case (Left, Right) => throw new IllegalArgumentException("Cannot convert coordinates from left to right")
        case (Left, Left) => throw new IllegalArgumentException("Cannot convert coordinates from left to left")
        case (Left, Back) => Coord(diceSize - 1 - coord.y, diceSize - 1)
        case (Left, Up) => Coord(0, diceSize - 1 - coord.x)
        case (Left, Down) => Coord(diceSize - 1, diceSize - 1 - coord.x)

        case (Back, Front) => throw new IllegalArgumentException("Cannot convert coordinates from back to front")
        case (Back, Right) => Coord(diceSize - 1, diceSize - 1 - coord.y)
        case (Back, Left) => Coord(0, diceSize - 1 - coord.y)
        case (Back, Back) => throw new IllegalArgumentException("Cannot convert coordinates from back to back")
        case (Back, Up) => Coord(diceSize - 1 - coord.x, diceSize - 1)
        case (Back, Down) => Coord(diceSize - 1 - coord.x, 0)

        case (Up, Front) => Coord(diceSize - 1 - coord.x, coord.y)
        case (Up, Right) => Coord(coord.y, diceSize - 1 - coord.x)
        case (Up, Left) => Coord(diceSize - 1 - coord.y, coord.x)
        case (Up, Back) => Coord(coord.x, diceSize - 1 - coord.y)
        case (Up, Up) => throw new IllegalArgumentException("Cannot convert coordinates from up to up")
        case (Up, Down) => throw new IllegalArgumentException("Cannot convert coordinates from up to down")

        case (Down, Front) => Coord(coord.x, diceSize - 1 - coord.y)
        case (Down, Right) => Coord(diceSize - 1 - coord.y, coord.x)
        case (Down, Left) => Coord(coord.y, diceSize - 1 - coord.x)
        case (Down, Back) => Coord(diceSize - 1 - coord.x, coord.y)
        case (Down, Up) => throw new IllegalArgumentException("Cannot convert coordinates from down to up")
        case (Down, Down) => throw new IllegalArgumentException("Cannot convert coordinates from down to down")
    }
}

@tailrec
final def applyTransformationToFacing(facing: Facing, transformations: List[FaceTransformation]): Facing = {
    transformations match {
        case Nil => facing
        case transformation :: tail =>
            val newFacing = (transformation, facing) match {
                case (Clockwise, North) => East
                case (Clockwise, East) => South
                case (Clockwise, South) => West
                case (Clockwise, West) => North
                case (CounterClockwise, North) => West
                case (CounterClockwise, West) => South
                case (CounterClockwise, South) => East
                case (CounterClockwise, East) => North
            }

            applyTransformationToFacing(newFacing, tail)
    }
}

final def changeSide(state: StateP2): Option[StateP2] = {
    val StateP2(face, going, coord) = state

    val FaceInstance(newFace, transformation) = simpleDiceFace(face, going)
    val newCoordinate = convertCoordinates(coord, face, newFace)
    val newGoing = applyTransformationToFacing(going, transformation)

    val destCell = faces(newFace).get(newCoordinate)

    destCell match {
        case Some(Empty(_)) => Some(state.copy(face = newFace, going = newGoing, coord = newCoordinate))
        case _ => None
    }
}

@tailrec
final def move(state: StateP2, n: Int): StateP2 = {
    val coords = untilFaceEnd(state.coord, state.going)
    coords match {
        case x #:: tail =>
            val c = faces(state.face).get(x)
            c match {
                case Some(Empty(_)) => move(state.copy(coord = x), n - 1)
                case Some(Wall) => state
                case _ => state
            }
        case _ => state
    }
}

final def exec(state: StateP2, instruction: Instruction): StateP2 = {
    val StateP2(face, going, coord) = state

    instruction match {
        case Move(n) => move(state, n)
        case TurnLeft =>
            val newGoing = turnMapping(going, TurnLeft)
            state.copy(going = newGoing)
        case TurnRight =>
            val newGoing = turnMapping(going, TurnRight)
            state.copy(going = newGoing)
    }
}

val part2State = instructions.foldLeft(initialP2)(exec)