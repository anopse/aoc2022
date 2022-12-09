

val lines = scala.io.Source.fromFile("input.txt").getLines.toList

sealed trait Move
case class Up(count: Int) extends Move
case class Down(count: Int) extends Move
case class Left(count: Int) extends Move
case class Right(count: Int) extends Move

def parseMove(s: String): Move = {
  val count = s.drop(2).toInt

  s.head match {
    case 'U' => Up(count)
    case 'D' => Down(count)
    case 'L' => Left(count)
    case 'R' => Right(count)
  }
}

val moves = lines.map(parseMove)

case class Coord(x: Int, y: Int)

sealed trait BasicMove {
    def apply(position: Coord): Coord
}
case object Up1 extends BasicMove {
    def apply(position: Coord): Coord = Coord(position.x, position.y - 1)
}
case object Down1 extends BasicMove {
    def apply(position: Coord): Coord = Coord(position.x, position.y + 1)
}
case object Left1 extends BasicMove {
    def apply(position: Coord): Coord = Coord(position.x - 1, position.y)
}
case object Right1 extends BasicMove {
    def apply(position: Coord): Coord = Coord(position.x + 1, position.y)
}

def toBasicMoves(move: Move): List[BasicMove] = move match {
  case Up(count) => List.fill(count)(Up1)
  case Down(count) => List.fill(count)(Down1)
  case Left(count) => List.fill(count)(Left1)
  case Right(count) => List.fill(count)(Right1)
}

val basicMoves = moves.flatMap(toBasicMoves)

sealed trait TailPosition
case object TailUp extends TailPosition
case object TailDown extends TailPosition
case object TailLeft extends TailPosition
case object TailRight extends TailPosition
case object TailUpLeft extends TailPosition
case object TailUpRight extends TailPosition
case object TailDownLeft extends TailPosition
case object TailDownRight extends TailPosition
case object TailSame extends TailPosition

def follow(position: TailPosition, move: BasicMove): TailPosition = (position, move) match {
    case (TailSame, Up1) => TailDown
    case (TailSame, Down1) => TailUp
    case (TailSame, Left1) => TailRight
    case (TailSame, Right1) => TailLeft

    // .T.
    // .H.
    case (TailUp, Up1) => TailSame
    case (TailUp, Down1) => TailUp
    case (TailUp, Left1) => TailUpRight
    case (TailUp, Right1) => TailUpLeft

    // .H.
    // .T.
    case (TailDown, Up1) => TailDown
    case (TailDown, Down1) => TailSame
    case (TailDown, Left1) => TailDownRight
    case (TailDown, Right1) => TailDownLeft

    // TH
    case (TailLeft, Up1) => TailDownLeft
    case (TailLeft, Down1) => TailUpLeft
    case (TailLeft, Left1) => TailSame
    case (TailLeft, Right1) => TailLeft

    // HT
    case (TailRight, Up1) => TailDownRight
    case (TailRight, Down1) => TailUpRight
    case (TailRight, Left1) => TailRight
    case (TailRight, Right1) => TailSame

    // T.
    // .H
    case (TailUpLeft, Up1) => TailLeft // cancel one part case
    case (TailUpLeft, Down1) => TailUp // cancel the perpendicular part case
    case (TailUpLeft, Left1) => TailUp // cancel one part case
    case (TailUpLeft, Right1) => TailLeft // cancel the perpendicular part case

    // .T
    // H.
    case (TailUpRight, Up1) => TailRight // cancel one part case
    case (TailUpRight, Down1) => TailUp // cancel the perpendicular part case
    case (TailUpRight, Left1) => TailRight // cancel the perpendicular part case
    case (TailUpRight, Right1) => TailUp // cancel one part case

    // .H
    // T.
    case (TailDownLeft, Up1) => TailDown // cancel the perpendicular part case
    case (TailDownLeft, Down1) => TailLeft // cancel one part case
    case (TailDownLeft, Left1) => TailDown // cancel one part case
    case (TailDownLeft, Right1) => TailLeft // cancel the perpendicular part case

    // H.
    // .T
    case (TailDownRight, Up1) => TailDown // cancel the perpendicular part case
    case (TailDownRight, Down1) => TailRight // cancel one part case
    case (TailDownRight, Left1) => TailRight // cancel the perpendicular part case
    case (TailDownRight, Right1) => TailDown // cancel one part case
}

def headCoordinateToTailCoordinate(headPos: Coord, tailPos: TailPosition): Coord = tailPos match {
    case TailUp => Coord(headPos.x, headPos.y - 1)
    case TailDown => Coord(headPos.x, headPos.y + 1)
    case TailLeft => Coord(headPos.x - 1, headPos.y)
    case TailRight => Coord(headPos.x + 1, headPos.y)
    case TailUpLeft => Coord(headPos.x - 1, headPos.y - 1)
    case TailUpRight => Coord(headPos.x + 1, headPos.y - 1)
    case TailDownLeft => Coord(headPos.x - 1, headPos.y + 1)
    case TailDownRight => Coord(headPos.x + 1, headPos.y + 1)
    case TailSame => headPos
}

case class State(headPos: Coord, tailPos: TailPosition)

def applyMove(state: State, move: BasicMove): State = {
    val newHeadPos = move(state.headPos)
    val newTailPos = follow(state.tailPos, move)
    println(s"$state + $move = ${State(newHeadPos, newTailPos)} (tail ${headCoordinateToTailCoordinate(newHeadPos, newTailPos)})")
    State(newHeadPos, newTailPos)
}

import annotation.tailrec
@tailrec
final def compute_visited(initialState: State, moveToDo: List[BasicMove], visited: Set[Coord]): Set[Coord] = {
    moveToDo match {
        case Nil => visited
        case move :: tail => {
            val newState = applyMove(initialState, move)
            val tailCoord = headCoordinateToTailCoordinate(newState.headPos, newState.tailPos)
            val newVisited = visited + tailCoord
            compute_visited(newState, tail, newVisited)
        }
    }
}

val all_tail_visited = compute_visited(State(Coord(0, 0), TailSame), basicMoves, Set.empty)

val part1 = all_tail_visited.size