

val lines = scala.io.Source.fromFile("input.txt").getLines.toList
val numbers = lines.map(_.map(_.toString().toInt).toList)
val marked = numbers.map(_.map(_ => false).toArray).toArray

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
val columnCount = numbers.head.length

// leftToRight
mark(numbers, identity)
// rightToLeft
mark(numbers.map(_.reverse), (x, y) => (columnCount - x - 1, y))
// topToBottom
mark(numbers.transpose, (x, y) => (y, x))
// bottomToTop
mark(numbers.transpose.map(_.reverse), (x, y) => (y, lineCount - x - 1))

val countMarked = marked.map(_.count(_ == true)).sum

// display board
for y <- 0 until lineCount do
    for x <- 0 until columnCount do
        print(if marked(y)(x) then "X" else numbers(y)(x))
    println()

println(s"Part 1: $countMarked")

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
    val leftToRight = score(start, l(y).drop(x+1))
    val rightToLeft = score(start, l(y).reverse.drop(columnCount - x))
    val topToBottom = score(start, l.transpose(implicitly)(x).drop(y + 1))
    val bottomToTop = score(start, l.transpose(implicitly)(x).reverse.drop(lineCount - y))
    val s = leftToRight * rightToLeft * topToBottom * bottomToTop
    print(s"($x, $y) ($start) : ")
    print(s"$leftToRight (${l(y).drop(x+1).mkString(",")}) * ")
    print(s"$rightToLeft (${l(y).reverse.drop(columnCount - x).mkString(",")}) * ")
    print(s"$topToBottom (${l.transpose(implicitly)(x).drop(y + 1).mkString(",")}) * ")
    print(s"$bottomToTop (${l.transpose(implicitly)(x).reverse.drop(lineCount - y).mkString(",")}) = ")
    println(s)
    s

val scores =
    for
        y <- 0 until lineCount
        x <- 0 until columnCount
        score = scoreAllDir(numbers, x, y)
    yield score

val maxScore = scores.max

