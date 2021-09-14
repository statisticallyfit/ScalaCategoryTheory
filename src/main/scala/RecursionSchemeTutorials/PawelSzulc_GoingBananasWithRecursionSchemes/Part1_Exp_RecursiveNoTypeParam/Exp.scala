package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part1_Exp_RecursiveNoTypeParam

/**
 *
 */
sealed trait Exp
final case class IntValue(v: Int) extends Exp
final case class DecValue(v: Double) extends Exp
final case class Sum(exp1: Exp, exp2: Exp) extends Exp
final case class Multiply(exp1: Exp, exp2: Exp) extends Exp
final case class Divide(exp1: Exp, exp2: Exp) extends Exp
final case class Square(exp: Exp) extends Exp

object Exp {

	val evaluate: Exp => Double = exp => exp match {
		case IntValue(v) => v.toDouble
		case DecValue(v) => v
		case Sum(e1, e2) => evaluate(e1) + evaluate(e2) // recursion part
		case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)
		case Square(e) => {
			val v = evaluate(e)
			v * v
		}
		case Divide(e1, e2) => evaluate(e1) / evaluate(e2)
	}

	val mkString: Exp => String = exp => exp match {
		case IntValue(v) => v.toString
		case DecValue(v) => v.toString
		case Sum(e1, e2) => s"(${mkString(e1)} + ${mkString(e2)})"
		case Multiply(e1, e2) => s"(${mkString(e1)} * ${mkString(e2)})"
		case Square(e) => s"(${mkString(e)}^2)"
		case Divide(e1, e2) => s"(${mkString(e1)} / ${mkString(e2)})"

	}

	val optimize: Exp => Exp = exp => exp match {
		case Multiply(e1, e2) if(e1 == e2) => Square(optimize(e1))
		case Multiply(e1, e2) => Multiply(optimize(e1), optimize(e2))
		case IntValue(v) => IntValue(v)
		case DecValue(v) => DecValue(v)
		case Sum(e1, e2) => Sum(optimize(e1), optimize(e2))
		case Square(e) => Square(optimize(e))
		case Divide(e1, e2) => Divide(optimize(e1), optimize(e2))
	}
}

object ExpRunner extends App {

	import Exp._

	import org.scalactic._
	import TripleEquals._
	import Tolerance._


	val EPSILON: Double = 0.00001

	val exp = Multiply(
		Sum(IntValue(10), DecValue(2.5)),
		Divide(DecValue(5.2), Sum(IntValue(10), IntValue(5)))
	)

	val doubledExp = Multiply(
		Sum(IntValue(3), IntValue(4)),
		Sum(IntValue(3), IntValue(4))
	)
	val squared = Square(Sum(IntValue(3),IntValue(4)))

	// TESTING: evaluate
	// NOTE: these ways of phrasing the +- are equivalent:
	// KEY: the epsilon value must be with fewer decimal places to the right than the solution number.
	assert( (evaluate(exp) - 4.3333333) < EPSILON, "Test: evaluate")
	assert(evaluate(exp) === 4.3333333 +- EPSILON, "Test 2: evaluate")
	assert(evaluate(doubledExp) == evaluate(squared), "Test 3: evaluate")


	// TESTING: mkstring
	assert(mkString(exp) == "((10 + 2.5) * (5.2 / (10 + 5)))", "Test: mkString")
	assert(mkString(doubledExp) == "((3 + 4) * (3 + 4))", "Test 2: mkString")
	assert(mkString(squared) == "((3 + 4)^2)", "Test 3: mkString")

	//TESTING: optimize
	assert(optimize(exp) == exp, "Test 1: optimize")
	assert(optimize(doubledExp) == squared, "Test 2: optimize")


}