import scala.util.parsing.combinator._
import scala.annotation.tailrec

val debugPrint = false
val isSampleInput = false

def dprintln(o: => Any) = if (debugPrint) println(o)
def dprintln() = if (debugPrint) println()
def dprint(o: => Any) = if (debugPrint) print(o)

val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).getLines().map(_.toInt).toArray


// Input format :
// 1
// 2
// -3
// 3
// -2

final def move[T](arr: Array[T], from: Int, to: Int): Unit = {
    dprintln(s"move from ${from} to ${to} len ${arr.length}")
    if (from < to) {
      val tmp = arr(from)
      Array.copy(arr, from + 1, arr, from, to - from)
      arr(to) = tmp
    } else if (from > to) {
      val tmp = arr(from)
      Array.copy(arr, to, arr, to + 1, from - to)
      arr(to) = tmp
    }
}

@tailrec
final def moveCircular[T](arr: Array[T], from: Long, to: Long): Unit = {
    dprintln(s"cmove from ${from} to ${to} len ${arr.length}")
    if (to >= arr.length) moveCircular(arr, from, to % (arr.length - 1))
    else if (to < 0) moveCircular(arr, from, to % (arr.length - 1) + (arr.length - 1))
    //else if (to == 0) move(arr, from, arr.length - 1)
    //else if (to == arr.length - 1) move(arr, from, 0)
    else move(arr, from.toInt, to.toInt)
}

def printArray[T](arr: Array[T]): Unit = {
    dprintln(arr.mkString(", "))
}

// part 1
val part1Array = input.indices.toArray

dprintln("Initial arrangement:")
printArray(part1Array)

for {
    i <- 0 until input.length
} {
    val n = input(i)
    val index = part1Array.indexOf(i)
    dprintln(s"${n} moves ... ${index} -> ${index + n} len ${input.length}")
    moveCircular(part1Array, index, index + n)
    printArray(part1Array)
}

printArray(part1Array.map(input(_)))

val part1ResultingArray = LazyList.continually(part1Array).flatten.take(input.length + 3001).toArray
val zeroIndex = part1ResultingArray.indexOf(input.indexOf(0))
val first = part1ResultingArray(zeroIndex + 1000)
val second = part1ResultingArray(zeroIndex + 2000)
val third = part1ResultingArray(zeroIndex + 3000)
val part1Result = input(first) + input(second) + input(third)

// part 2

val part2Array = input.indices.toArray
val part2Input = input.map(_ * 811589153L)

dprintln("Initial arrangement:")
printArray(part2Input)


part2Array.map(part2Input(_)).take(10).mkString(", ")

for {
    _ <- 0 until 10
    i <- 0 until input.length
} {
    val n = part2Input(i)
    val index = part2Array.indexOf(i)
    //dprintln(s"${n} moves ... ${index} -> ${index + n} len ${part2Array.length}")
    moveCircular(part2Array, index, index + n)
    printArray(part2Array)
}

printArray(part2Array.map(part2Input(_)))

part2Array.map(part2Input(_)).take(10).mkString(", ")


val part2ResultingArray = LazyList.continually(part2Array).flatten.take(part2Array.length + 3001).toArray
val part2zeroIndex = part2ResultingArray.indexOf(input.indexOf(0))
val part2first = part2Input(part2ResultingArray(part2zeroIndex + 1000))
val part2second = part2Input(part2ResultingArray(part2zeroIndex + 2000))
val part2third = part2Input(part2ResultingArray(part2zeroIndex + 3000))
val part2Result = part2first + part2second + part2third