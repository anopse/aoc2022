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
//  1=-0-2
//  12111
//  2=0=

type Number = Long

sealed case class Snafu(s: String) {
    def toNumber: Number = {
        val rs = s.reverse
        var pos = 0
        var result: Number = 0
        for (c <- rs) {
            val digit = Snafu.snafuDigitToDecimal(c)
            result += digit * Math.pow(Snafu.pow, pos).toLong
            pos += 1
        }

        result
    }

    def incr: Snafu = {
        val rs = s.reverse.toList

        def inc(dl: List[Char]): List[Char] = {
            dl match {
                case Nil => List('1')
                case '2' :: tail => '=' :: inc(tail)
                case '1' :: tail => '2' :: tail
                case '0' :: tail => '1' :: tail
                case '-' :: tail => '0' :: tail
                case '=' :: tail => '-' :: tail
                case _ => throw new Exception(s"Invalid digit ${dl.head}")
            }
        }

        val r = inc(rs)
        Snafu(r.mkString.reverse)
    }
}

object Snafu {
    val digits = List('=', '-', '0', '1', '2')

    val pow = digits.length

    def snafuDigitToDecimal(c: Char): Number = {
        c match {
            case '=' => -2
            case '-' => -1
            case '0' => 0
            case '1' => 1
            case '2' => 2
            case _ => throw new Exception(s"Invalid digit $c")
        }
    }

    private def fromNegNumber(n: Number): Snafu = {
        val matches = for {
            i <- 0 to 100
            h <- List("=", "-")
            min = "=" * i
            sMin = h + min
            snafuMin = Snafu(sMin)
            decMin = snafuMin.toNumber
            max = "2" * i
            sMax = h + max
            snafuMax = Snafu(sMax)
            decMax = snafuMax.toNumber
            if decMin <= n && n <= decMax
        } yield i -> h

        matches.headOption match {
            case Some((i, h)) => {
                val mid = "0" * i
                val sMid = h + mid
                val snafuMid = Snafu(sMid)
                val decMid = snafuMid.toNumber
                if (decMid == n)
                    return snafuMid
                else {
                    val subSnafu = Snafu.fromNumber(n - decMid)
                    val subLength = subSnafu.s.length()
                    val pad = "0" * (i - subLength)
                    Snafu(h + pad + subSnafu.s)
                }
            }
            case None => throw new Exception(s"Invalid number $n")
        }
    }

    private def fromPosNumber(n: Number): Snafu = {
        val matches = for {
            i <- 0 to 100
            h <- List("1", "2")
            min = "=" * i
            sMin = h + min
            snafuMin = Snafu(sMin)
            decMin = snafuMin.toNumber
            max = "2" * i
            sMax = h + max
            snafuMax = Snafu(sMax)
            decMax = snafuMax.toNumber
            if decMin <= n && n <= decMax
        } yield i -> h

        matches.headOption match {
            case Some((i, h)) => {
                val mid = "0" * i
                val sMid = h + mid
                val snafuMid = Snafu(sMid)
                val decMid = snafuMid.toNumber
                if (decMid == n)
                    return snafuMid
                else {
                    val subSnafu = Snafu.fromNumber(n - decMid)
                    val subLength = subSnafu.s.length()
                    val pad = "0" * (i - subLength)
                    Snafu(h + pad + subSnafu.s)
                }
            }
            case None => throw new Exception(s"Invalid number $n")
        }
    }

    def fromNumber(n: Number): Snafu = {
        val r = if (n == 0) {
            Snafu("0")
        } else if (n > 0) {
            fromPosNumber(n)
        } else {
            fromNegNumber(n)
        }

        dprintln(s"fromNumber($n) = $r")
        r
    }
}

val inputSnafu = input.map(Snafu.apply)
val inputDec = inputSnafu.map(_.toNumber)
val inputSum = inputDec.sum
val inputSumSnafu = Snafu.fromNumber(inputSum)
val inputSumDec = inputSumSnafu.toNumber