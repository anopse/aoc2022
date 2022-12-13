import scala.util.parsing.combinator._
import scala.annotation.tailrec

val input = scala.io.Source.fromFile("input.txt").mkString

// Input format:
// [1,1,3,1,1]
// [1,1,5,1,1]
// 
// [[1],[2,3,4]]
// [[1],4]

sealed trait Tree
case class Leaf(value: Int) extends Tree
case class Node(children: List[Tree]) extends Tree

// separated by a new line
case class Pair(left: Tree, right: Tree)
// separated by a blank line
type Problem = List[Pair]

object Parser extends RegexParsers with PackratParsers {
    lazy val leaf: PackratParser[Leaf] = """\d+""".r ^^ { d => Leaf(d.toInt) }
    lazy val node: PackratParser[Node] = "[" ~> repsep(tree, ",") <~ "]" ^^ Node.apply
    lazy val tree: PackratParser[Tree] = leaf | node

    lazy val pair: PackratParser[Pair] = tree ~ tree ^^ { case l ~ r => Pair(l, r) }
    lazy val problem: PackratParser[Problem] = repsep(pair, "")
}

val problem = Parser.parseAll(Parser.problem, input)

final def compare(left: Tree, right: Tree): Int = (left, right) match {
    case (Leaf(l), Leaf(r)) => l - r
    case (Node(_), Leaf(r)) => compare(left, Node(List(Leaf(r))))
    case (Leaf(l), Node(_)) => compare(Node(List(Leaf(l))), right)
    case (Node(Nil), Node(Nil)) => 0
    case (Node(Nil), Node(_::_)) => -1
    case (Node(_::_), Node(Nil)) => 1
    case (Node(lh::lt), Node(rh::rt)) =>
        val c = compare(lh, rh)
        if (c == 0) compare(Node(lt), Node(rt))
        else c
}

val problems = problem.get.zipWithIndex

for ((p, i) <- problems) {
    val c = compare(p.left, p.right)
    println(s"Case #${i+1}: ${if (c < 0) "Left" else if (c > 0) "Right" else "Tie"}")
}

val left_indexes = problem.get.zipWithIndex.filter(p => compare(p._1.left, p._1.right) < 0).map(_._2 + 1)

val part1 = left_indexes.sum

// part 2

val input_packets = problem.get.map(p => List(p.left, p.right)).flatten

val separators_packets = List(
    Node(List(Node(List(Leaf(2))))),
    Node(List(Node(List(Leaf(6)))))
)

val all_packets = input_packets ++ separators_packets

val sorted_packets = all_packets.sortWith((a, b) => compare(a, b) < 0)

val separators_indexes = separators_packets.map(p => sorted_packets.indexOf(p) + 1)

val part2 = separators_indexes.product