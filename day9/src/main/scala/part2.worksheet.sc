


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

case class Coord(x: Int, y: Int) {
  def withX(newX: Int) = Coord(newX, y)
  def withY(newY: Int) = Coord(x, newY)
}

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

type Snake = List[Coord]

def newSnake(length: Int) = List.fill(length)(Coord(0, 0))

def moveHead(snake: Snake, move: BasicMove): Snake = {
  val newHead = move(snake.head)
  newHead :: snake.tail
}

def followStep(head: Coord, tail: Coord): Coord = {
  val xDiff = head.x - tail.x
  val yDiff = head.y - tail.y
  val xDist = Math.abs(xDiff)
  val yDist = Math.abs(yDiff)
  val dist = Math.max(xDist, yDist)
  val closerX = if (xDiff > 0) 1 else -1
  val closerY = if (yDiff > 0) 1 else -1

  dist match {
    case 0 | 1 => tail
    case x => {
      (xDist, yDist) match {
        case (0, _) => tail.withY(tail.y + closerY)
        case (_, 0) => tail.withX(tail.x + closerX)
        case _ => tail.withX(tail.x + closerX).withY(tail.y + closerY)
      }
    }
  }
}

def follow(snake: Snake): Snake = snake match {
    case Nil => Nil
    case head :: Nil => head :: Nil
    case head :: tail :: Nil => head::followStep(head, tail)::Nil
    case head :: mid :: rest => {
      val newMid = followStep(head, mid)
      head::follow(newMid::rest)
    }
}

def move(snake: Snake, move: BasicMove): Snake = {
  val newSnake = moveHead(snake, move)
  follow(newSnake)
}

def computeTailPositions(snake: Snake, moves: List[BasicMove]): List[Coord] = {
  moves.foldLeft(snake -> List.empty[Coord])((acc, basicMove) => {
    val s = acc._1
    val list = acc._2
    val newSnake = move(s, basicMove)
    //println(newSnake)
    newSnake -> (newSnake.last :: list)
  })._2
}

def solveFor(len: Int, moves: List[BasicMove]): Int = {
  val snake = newSnake(len)
  val tailPositions = computeTailPositions(snake, basicMoves)
  //println(tailPositions)
  tailPositions.toSet.size
}

val part1 = solveFor(2, basicMoves)
val part2 = solveFor(10, basicMoves)