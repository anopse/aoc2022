import scala.util.parsing.combinator._

val input = scala.io.Source.fromFile("input.txt").mkString

// Monkey 0:
//  Starting items: 79, 98
//  Operation: new = old * 19
//  Test: divisible by 23
//    If true: throw to monkey 2
//    If false: throw to monkey 3

type Item = Int
type Items = List[Item]

sealed trait ArithmeticExpr
case class Literal(value: Int) extends ArithmeticExpr
case class Add(left: ArithmeticExpr, right: ArithmeticExpr) extends ArithmeticExpr
case class Multiply(left: ArithmeticExpr, right: ArithmeticExpr) extends ArithmeticExpr
case object Var extends ArithmeticExpr

case class Test(divisableBy: Int, ifTrueThrowTo: Int, ifFalseThrowTo: Int)

case class MonkeyExpr(monkeyId: Int, items: Items, operation: ArithmeticExpr, test: Test)

object Parser extends RegexParsers with PackratParsers {
    val item: PackratParser[Item] = """\d+""".r ^^ { _.toInt }
    val items: PackratParser[Items] = rep1sep(item, ",")

    val itemLine: PackratParser[Items] = "Starting items:" ~> items

    lazy val literal: PackratParser[Literal] = """\d+""".r ^^ { s => Literal(s.toInt) }
    lazy val varExpr: PackratParser[Var.type] = "old" ^^ { _ => Var }
    lazy val AddOp: PackratParser[Add] = arithmeticExpr ~ "+" ~ arithmeticExpr ^^ { case l ~ _ ~ r => Add(l, r) }
    lazy val MultiplyOp: PackratParser[Multiply] = arithmeticExpr ~ "*" ~ arithmeticExpr ^^ { case l ~ _ ~ r => Multiply(l, r) }
    lazy val arithmeticExpr: PackratParser[ArithmeticExpr] = AddOp | MultiplyOp | literal | varExpr

    val operationLine: PackratParser[ArithmeticExpr] = "Operation:" ~> "new" ~> "=" ~> arithmeticExpr

    val divisibleBy: PackratParser[Int] = "divisible by" ~> """\d+""".r ^^ { _.toInt }
    val ifTrueThrowTo: PackratParser[Int] = "If true:" ~> "throw to monkey" ~> """\d+""".r ^^ { _.toInt }
    val ifFalseThrowTo: PackratParser[Int] = "If false:" ~> "throw to monkey" ~> """\d+""".r ^^ { _.toInt }

    val testLine: PackratParser[Test] = "Test:" ~> divisibleBy ~ ifTrueThrowTo ~ ifFalseThrowTo ^^ { case d ~ t ~ f => Test(d, t, f) }

    val monkeyIdLine: PackratParser[Int] = "Monkey" ~> """\d+""".r <~ ":" ^^ { _.toInt }

    val monkeyExpr: PackratParser[MonkeyExpr] = monkeyIdLine ~ itemLine ~ operationLine ~ testLine ^^ { case m ~ i ~ o ~ t => MonkeyExpr(m, i, o, t) }

    // separated by newlines
    val monkeys: PackratParser[List[MonkeyExpr]] = rep1sep(monkeyExpr, "")

}

val monkeysExpr = Parser.parseAll(Parser.monkeys, input)

case class MonkeyState(items: List[BigInt], operation: ArithmeticExpr, test: Test, inspectedCount: Int)

case class State(monkeys: Map[Int, MonkeyState])

def applyOperation(worry: BigInt, operation: ArithmeticExpr): BigInt = {
    operation match {
        case Literal(value) => value
        case Add(left, right) => applyOperation(worry, left) + applyOperation(worry, right)
        case Multiply(left, right) => applyOperation(worry, left) * applyOperation(worry, right)
        case Var => worry
    }
}

def applyTest(worry: BigInt, test: Test): Int = if (worry % test.divisableBy == 0) test.ifTrueThrowTo else test.ifFalseThrowTo

def processMonkey(state: State, key: Int, worryDivisor: Int): State = {
    val monkeyState = state.monkeys(key)
    monkeyState.items match {
        case Nil => state
        case itemWorry :: rest =>
            val newWorry = applyOperation(itemWorry, monkeyState.operation) / worryDivisor
            val newInspectedCount = monkeyState.inspectedCount + 1
            val newMonkeyState = monkeyState.copy(items = rest, inspectedCount = newInspectedCount)
            val throwTo = applyTest(newWorry, monkeyState.test)
            val targetMonkey = state.monkeys(throwTo)
            val newTargetMonkeyState = targetMonkey.copy(items = targetMonkey.items :+ newWorry)
            val newMonkeys = state.monkeys + (key -> newMonkeyState) + (throwTo -> newTargetMonkeyState)
            val newState = State(newMonkeys)
            processMonkey(newState, key, worryDivisor)
    }
}

def processRound(state: State, worryDivisor: Int): State = {
    state.monkeys.keys.toList.sorted.foldLeft(state) { (state, key) =>
        processMonkey(state, key, worryDivisor)
    }
}

val initialState = State(monkeysExpr.get.map(m => m.monkeyId -> MonkeyState(m.items.map(BigInt.apply), m.operation, m.test, 0)).toMap)

// part 1

val round = 20

val finalState = (0 until round).foldLeft(initialState) { (state, r) =>
    val s = processRound(state, worryDivisor = 3)
    println(s"After round ${r+1}, the monkeys are holding items with these worry levels:")
    for {
        key <- state.monkeys.keys.toList.sorted
        monkeyState = s.monkeys(key)
    } {
        println(s"Monkey $key: ${monkeyState.items.mkString(", ")}")
    }
    println()
    s
}

val inspecteds = finalState.monkeys.values.map(_.inspectedCount).toList.sorted.reverse

val part1 = inspecteds.take(2).product

// part 2

val round2 = 10000
val gcd = monkeysExpr.get.map(_.test.divisableBy).map(BigInt.apply).product

val finalState2 = (0 until round2).foldLeft(initialState) { (state, r) =>
    val s = processRound(state, worryDivisor = 1)
    val reducedS = s.copy(monkeys = s.monkeys.map { case (key, monkeyState) =>
        val newItems = monkeyState.items.map(_ % gcd)
        key -> monkeyState.copy(items = newItems)
    })
    //println(s"After round ${r+1}, the monkeys are holding items with these worry levels:")
    // for {
    //     key <- state.monkeys.keys.toList.sorted
    //     monkeyState = s.monkeys(key)
    // } {
    //     println(s"Monkey $key: ${monkeyState.items.mkString(", ")}")
    // }
    // println()
    reducedS
}

val inspecteds2 = finalState2.monkeys.values.map(_.inspectedCount).toList.sorted.reverse

val part2 = inspecteds2.take(2).map(BigInt.apply).product