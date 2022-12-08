


val lines = scala.io.Source.fromFile("input.txt").getLines.toList
// lines: List[String] = List(30373, 25512, 65332, 33549, 35390)
val numbers = lines.map(_.map(_.toString().toInt).toList)
// numbers: List[List[Int]] = List(List(3, 0, 3, 7, 3), List(2, 5, 5, 1, 2), List(6, 5, 3, 3, 2), List(3, 3, 5, 4, 9), List(3, 5, 3, 9, 0))
val marked = numbers.map(_.map(_ => false).toArray).toArray
// marked: Array[Array[Boolean]] = [[Z@1b222c68

def mark(l: List[List[Int]], translate: ((Int, Int)) => (Int, Int)): Unit =
    for
        y <- 0 until l.length
        x <- 0 until l.head.length
        (tx, ty) = translate(x, y)
        if marked(ty)(tx) == false
        if (0 until x).forall(xPrev => l(y)(xPrev) < l(y)(x))
    do
        marked(ty)(tx) = true

val lineCount = numbers.length
// lineCount: Int = 5
val columnCount = numbers.head.length
// columnCount: Int = 5

// leftToRight
mark(numbers, identity)
// rightToLeft
mark(numbers.map(_.reverse), (x, y) => (columnCount - x - 1, y))
// topToBottom
mark(numbers.transpose, (x, y) => (y, x))
// bottomToTop
mark(numbers.transpose.map(_.reverse), (x, y) => (y, lineCount - x - 1))

val countMarked = marked.map(_.count(_ == true)).sum
// countMarked: Int = 21

for y <- 0 until lineCount do
    for x <- 0 until columnCount do
        print(if marked(y)(x) then "X" else numbers(y)(x))
    println()
// XXXXX
// XXX1X
// XX3XX
// X3X4X
// XXXXX

println(s"Part 1: $countMarked")
// Part 1: 21

def score(start: Int, l: List[Int]): Int =
    var stop = false
    var count = 0
    for h <- l
        if !stop
    do
        if h >= start then
            stop = true
        count += 1
    count

def scoreAllDir(l: List[List[Int]], x: Int, y: Int): Int =
    val start = l(y)(x)
    print(s"($x, $y) ($start) : ")
    val leftToRight = score(start, l(y).drop(x+1))
    print(s"$leftToRight (${l(y).drop(x+1).mkString(",")}) * ")
    val rightToLeft = score(start, l(y).reverse.drop(columnCount - x))
    print(s"$rightToLeft (${l(y).reverse.drop(columnCount - x).mkString(",")}) * ")
    val topToBottom = score(start, l.transpose(implicitly)(x).drop(y + 1))
    print(s"$topToBottom (${l.transpose(implicitly)(x).drop(y + 1).mkString(",")}) * ")
    val bottomToTop = score(start, l.transpose(implicitly)(x).reverse.drop(lineCount - y))
    print(s"$bottomToTop (${l.transpose(implicitly)(x).reverse.drop(lineCount - y).mkString(",")}) = ")
    val s = leftToRight * rightToLeft * topToBottom * bottomToTop
    println(s)
    s

val scores =
    for
        y <- 0 until lineCount
        x <- 0 until columnCount
        score = scoreAllDir(numbers, x, y)
    yield score
// scores: IndexedSeq[Int] = Vector(0, 0, 0, 0, 0, 0, 1, 4, 1, 0, 0, 6, 1, 2, 0, 0, 1, 8, 3, 0, 0, 0, 0, 0, 0)
// (0, 0) (3) : 2 (0,3,7,3) * 0 () * 2 (2,6,3,3) * 0 () = 0
// (1, 0) (0) : 1 (3,7,3) * 1 (3) * 1 (5,5,3,5) * 0 () = 0
// (2, 0) (3) : 1 (7,3) * 2 (0,3) * 1 (5,3,5,3) * 0 () = 0
// (3, 0) (7) : 1 (3) * 3 (3,0,3) * 4 (1,3,4,9) * 0 () = 0
// (4, 0) (3) : 0 () * 1 (7,3,0,3) * 3 (2,2,9,0) * 0 () = 0
// (0, 1) (2) : 1 (5,5,1,2) * 0 () * 1 (6,3,3) * 1 (3) = 0
// (1, 1) (5) : 1 (5,1,2) * 1 (2) * 1 (5,3,5) * 1 (0) = 1
// (2, 1) (5) : 2 (1,2) * 1 (5,2) * 2 (3,5,3) * 1 (3) = 4
// (3, 1) (1) : 1 (2) * 1 (5,5,2) * 1 (3,4,9) * 1 (7) = 1
// (4, 1) (2) : 0 () * 2 (1,5,5,2) * 1 (2,9,0) * 1 (3) = 0
// (0, 2) (6) : 4 (5,3,3,2) * 0 () * 2 (3,3) * 2 (2,3) = 0
// (1, 2) (5) : 3 (3,3,2) * 1 (6) * 2 (3,5) * 1 (5,0) = 6
// (2, 2) (3) : 1 (3,2) * 1 (5,6) * 1 (5,3) * 1 (5,3) = 1
// (3, 2) (3) : 1 (2) * 1 (3,5,6) * 1 (4,9) * 2 (1,7) = 2
// (4, 2) (2) : 0 () * 1 (3,3,5,6) * 1 (9,0) * 1 (2,3) = 0
// (0, 3) (3) : 1 (3,5,4,9) * 0 () * 1 (3) * 1 (6,2,3) = 0
// (1, 3) (3) : 1 (5,4,9) * 1 (3) * 1 (5) * 1 (5,5,0) = 1
// (2, 3) (5) : 2 (4,9) * 2 (3,3) * 1 (3) * 2 (3,5,3) = 8
// (3, 3) (4) : 1 (9) * 1 (5,3,3) * 1 (9) * 3 (3,1,7) = 3
// (4, 3) (9) : 0 () * 4 (4,5,3,3) * 1 (0) * 3 (2,2,3) = 0
// (0, 4) (3) : 1 (5,3,9,0) * 0 () * 0 () * 1 (3,6,2,3) = 0
// (1, 4) (5) : 2 (3,9,0) * 1 (3) * 0 () * 2 (3,5,5,0) = 0
// (2, 4) (3) : 1 (9,0) * 1 (5,3) * 0 () * 1 (5,3,5,3) = 0
// (3, 4) (9) : 1 (0) * 3 (3,5,3) * 0 () * 4 (4,3,1,7) = 0
// (4, 4) (0) : 0 () * 1 (9,3,5,3) * 0 () * 1 (9,2,2,3) = 0

val maxScore = scores.max
// maxScore: Int = 8