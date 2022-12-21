import scala.util.parsing.combinator._
import scala.annotation.tailrec

val debugPrint = true
val isSampleInput = false

def dprintln(o: => Any) = if (debugPrint) println(o)
def dprintln() = if (debugPrint) println()
def dprint(o: => Any) = if (debugPrint) print(o)

val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format :
// root: pppw + sjmn
// dbpl: 5
// cczh: sllz + lgvd

type Num = Long

sealed trait Formula
sealed case class Literal(value: Num) extends Formula
sealed case class Ref(name: String) extends Formula
sealed case class Sum(left: Formula, right: Formula) extends Formula
sealed case class Product(left: Formula, right: Formula) extends Formula
sealed case class Minus(left: Formula, right: Formula) extends Formula
sealed case class Division(left: Formula, right: Formula) extends Formula

sealed case class Monkey(
    id: String,
    formula: Formula
)

type Problem = List[Monkey]

object Parser extends RegexParsers with PackratParsers {
    lazy val number: Parser[Long] = """\d+""".r ^^ (_.toLong)
    lazy val id: Parser[String] = """[a-z]+""".r

    lazy val literal: Parser[Literal] = number ^^ Literal.apply
    lazy val ref: Parser[Ref] = id ^^ Ref.apply
    lazy val sum: PackratParser[Sum] = (formula | formula) ~ ("+" ~> (formula | formula)) ^^ { case left ~ right => Sum(left, right) }
    lazy val product: PackratParser[Product] = (formula | formula) ~ ("*" ~> (formula | formula)) ^^ { case left ~ right => Product(left, right) }
    lazy val minus: PackratParser[Minus] = (formula | formula) ~ ("-" ~> (formula | formula)) ^^ { case left ~ right => Minus(left, right) }
    lazy val division: PackratParser[Division] = (formula | formula) ~ ("/" ~> (formula | formula)) ^^ { case left ~ right => Division(left, right) }
    lazy val formula: PackratParser[Formula] = sum | product | minus | division | literal | ref
    lazy val monkey: PackratParser[Monkey] = id ~ (":" ~> formula) ^^ { case id ~ formula => Monkey(id, formula) }
    val problem: Parser[Problem] = monkey.+
}

val problemRaw = Parser.parseAll(Parser.problem, input)
val problem = problemRaw.get

val monkeyMap = problem.map(m => m.id -> m).toMap

final def eval(formula: Formula): Num = {
    formula match
        case Literal(value) => value
        case Ref(name) => eval(monkeyMap(name).formula)
        case Sum(left, right) => eval(left) + eval(right)
        case Product(left, right) => eval(left) * eval(right)
        case Minus(left, right) => eval(left) - eval(right)
        case Division(left, right) => eval(left) / eval(right)
}

val part1 = eval(monkeyMap("root").formula)

// part2

sealed trait Equation
sealed case class EqLiteral(value: Num) extends Equation
sealed case class EqSum(left: Equation, right: Equation) extends Equation
sealed case class EqProduct(left: Equation, right: Equation) extends Equation
sealed case class EqMinus(left: Equation, right: Equation) extends Equation
sealed case class EqDivision(left: Equation, right: Equation) extends Equation
sealed case class EqEqual(left: Equation, right: Equation) extends Equation
object EqUnkown extends Equation

final def equationFromFormula(formula: Formula): Equation = {
    formula match
        case Literal(value) => EqLiteral(value)
        case Ref("root") =>
            val rootFormula = monkeyMap("root").formula.asInstanceOf[Sum]
            EqEqual(equationFromFormula(rootFormula.left), equationFromFormula(rootFormula.right))
        case Ref("humn") => EqUnkown
        case Ref(name) => equationFromFormula(monkeyMap(name).formula)
        case Sum(left, right) => EqSum(equationFromFormula(left), equationFromFormula(right))
        case Product(left, right) => EqProduct(equationFromFormula(left), equationFromFormula(right))
        case Minus(left, right) => EqMinus(equationFromFormula(left), equationFromFormula(right))
        case Division(left, right) => EqDivision(equationFromFormula(left), equationFromFormula(right))
}

val rawEquation: EqEqual = equationFromFormula(Ref("root")).asInstanceOf[EqEqual]

final def isUnknownInEquation(equation: Equation): Boolean = {
    equation match
        case EqUnkown => true
        case EqSum(left, right) => isUnknownInEquation(left) || isUnknownInEquation(right)
        case EqProduct(left, right) => isUnknownInEquation(left) || isUnknownInEquation(right)
        case EqMinus(left, right) => isUnknownInEquation(left) || isUnknownInEquation(right)
        case EqDivision(left, right) => isUnknownInEquation(left) || isUnknownInEquation(right)
        case EqEqual(left, right) => isUnknownInEquation(left) || isUnknownInEquation(right)
        case _ => false
}

val isEqLeft = isUnknownInEquation(rawEquation.left)
val eqWithUnknownLeft = if isEqLeft then rawEquation else EqEqual(rawEquation.right, rawEquation.left)

final def rewriteEquation(eq: EqEqual): EqEqual = {
    eq match
        case EqEqual(EqSum(ll, lr), r) =>
            val llu = isUnknownInEquation(ll)
            llu match {
                case true => EqEqual(ll, EqMinus(r, lr))
                case false => EqEqual(lr, EqMinus(r, ll))
            }
        case EqEqual(EqMinus(ll, lr), r) =>
            val llu = isUnknownInEquation(ll)
            llu match {
                case true => EqEqual(ll, EqSum(r, lr))
                case false => EqEqual(lr, EqMinus(ll, r))
            }
        case EqEqual(EqProduct(ll, lr), r) =>
            val llu = isUnknownInEquation(ll)
            llu match {
                case true => EqEqual(ll, EqDivision(r, lr))
                case false => EqEqual(lr, EqDivision(r, ll))
            }
        case EqEqual(EqDivision(ll, lr), r) =>
            val llu = isUnknownInEquation(ll)
            llu match {
                case true => EqEqual(ll, EqProduct(r, lr))
                case false => EqEqual(lr, EqDivision(ll, r))
            }
        case _ => eq
}

final def isDoneRewrite(equation: EqEqual): Boolean = {
    equation match
        case EqEqual(EqUnkown, _) => true
        case _ => false
}

@tailrec
final def rerwiteEquationTotally(eq: EqEqual): EqEqual = {
    val newEq = rewriteEquation(eq)

    isDoneRewrite(newEq) match {
        case true => newEq
        case false => rerwiteEquationTotally(newEq)
    }
}

final def solveSimplifiedEquation(eq: Equation): Num = {
    eq match
        case EqEqual(EqUnkown, right) => solveSimplifiedEquation(right)
        case EqLiteral(value) => value
        case EqSum(left, right) => solveSimplifiedEquation(left) + solveSimplifiedEquation(right)
        case EqMinus(left, right) => solveSimplifiedEquation(left) - solveSimplifiedEquation(right)
        case EqProduct(left, right) => solveSimplifiedEquation(left) * solveSimplifiedEquation(right)
        case EqDivision(left, right) => solveSimplifiedEquation(left) / solveSimplifiedEquation(right)
        case _ => throw new Exception("not supported")
}

val part2Equation = rerwiteEquationTotally(eqWithUnknownLeft)
val part2 = solveSimplifiedEquation(part2Equation)