package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part2_ExpTypedInfinite

/**
 *
 */
sealed trait Exp[A]
final case class IntValue[A](v: Int) extends Exp[A]
final case class DecValue[A](v: Double) extends Exp[A]
final case class Sum[A](exp1: A, exp2: A) extends Exp[A]
final case class Multiply[A](exp1: A, exp2: A) extends Exp[A]
final case class Divide[A](exp1: A, exp2: A) extends Exp[A]
final case class Square[A](exp: A) extends Exp[A]

/*object ExpThings {

	def evaluate[A]: Exp[A] => Double = exp => exp match {
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

	def mkString[A]: Exp[A] => String = exp => exp match {
		case IntValue(v) => v.toString
		case DecValue(v) => v.toString
		case Sum(e1, e2) => s"(${mkString(e1)} + ${mkString(e2)})"
		case Multiply(e1, e2) => s"(${mkString(e1)} * ${mkString(e2)})"
		case Square(e) => s"(${mkString(e)}^2)"
		case Divide(e1, e2) => s"(${mkString(e1)} / ${mkString(e2)})"

	}

	def optimize[A]: Exp[A] => Exp[A] = exp => exp match {
		case Multiply(e1, e2) if(e1 == e2) => Square(optimize(e1))
		case Multiply(e1, e2) => Multiply(optimize(e1), optimize(e2))
		case IntValue(v) => IntValue(v)
		case DecValue(v) => DecValue(v)
		case Sum(e1, e2) => Sum(optimize(e1), optimize(e2))
		case Square(e) => Square(optimize(e))
		case Divide(e1, e2) => Divide(optimize(e1), optimize(e2))
	}
}*/

object ExpRunner2 extends App {

	//import ExpThings._

	import org.scalactic._
	import TripleEquals._
	import Tolerance._


	val EPSILON: Double = 0.00001




	// TESTING: with the recursive stacked types to show the problem of typed Exp[A]
	// stack two levels of Exp
	val two_STACK: Exp[Exp[Unit]] = Sum[Exp[Unit]](IntValue[Unit](10),IntValue[Unit](5) )

	//stack three levels of Exp
	val three_STACK: Exp[Exp[Exp[Unit]]] = Divide[Exp[Exp[Unit]]](
		DecValue[Exp[Unit]](5.2),
		Sum[Exp[Unit]](IntValue[Unit](10),IntValue[Unit](5) )
	)
	//NOTE: DecValue is assigned type param "Exp[Unit]" even though it would seem it just gets type param
	// "Unit"  like the IntValues. This is because we want the top type constructor Divide to get type
	// "Exp[Exp[Unit]]"




	// non-typed
	val exp = Multiply(
		Sum(IntValue(10), DecValue(2.5)),
		Divide(DecValue(5.2), Sum(IntValue(10), IntValue(5)))
	)
	// typed
	val exp_STACK: Exp[Exp[Exp[Exp[Unit]]]] = Multiply[Exp[Exp[Exp[Unit]]]]( //
		Sum[Exp[Exp[Unit]]](
			IntValue[Exp[Unit]](10),
			DecValue[Exp[Unit]](2.5)
		),
		Divide[Exp[Exp[Unit]]](
			DecValue[Exp[Unit]](5.2),
			Sum[Exp[Unit]](IntValue[Unit](10), IntValue[Unit](5))
		)
	)

	// non-typed
	val doubledExp = Multiply(
		Sum(IntValue(3), IntValue(4)),
		Sum(IntValue(3), IntValue(4))
	)
	// typed
	val doubledExp_STACK: Exp[Exp[Exp[Unit]]] = Multiply[Exp[Exp[Unit]]](
		Sum[Exp[Unit]](IntValue[Unit](3), IntValue[Unit](4)),
		Sum[Exp[Unit]](IntValue[Unit](3), IntValue[Unit](4))
	)

	//non-typed
	val squared = Square(Sum(IntValue(3),IntValue(4)))
	//typed
	val squared_STACK: Exp[Exp[Exp[Unit]]]  = Square[Exp[Exp[Unit]]](
		Sum[Exp[Unit]](IntValue[Unit](3),IntValue[Unit](4))
	)



	// TESTING: evaluate
	// NOTE: these ways of phrasing the +- are equivalent:
	// KEY: the epsilon value must be with fewer decimal places to the right than the solution number.
	//println(evaluate(exp_STACK) == evaluate(exp))


	/*assert( evaluate(exp_STACK) == evaluate(exp)
		&& (evaluate(exp) - 4.3333333) < EPSILON, "Test: evaluate")
	assert(evaluate(exp) === 4.3333333 +- EPSILON, "Test 2: evaluate")
	assert(evaluate(doubledExp) == evaluate(squared), "Test 3: evaluate")

	assert( (evaluate(exp_STACK) - 4.3333333) < EPSILON, "Test: evaluate STACK")
	assert(evaluate(exp_STACK) === 4.3333333 +- EPSILON, "Test 2: evaluate STACK")
	assert(evaluate(doubledExp_STACK) == evaluate(squared_STACK), "Test 3: evaluate STACK")


	// TESTING: mkstring
	assert(mkString(exp) == "((10 + 2.5) * (5.2 / (10 + 5)))", "Test: mkString")
	assert(mkString(doubledExp) == "((3 + 4) * (3 + 4))", "Test 2: mkString")
	assert(mkString(squared) == "((3 + 4)^2)", "Test 3: mkString")


	//TESTING: optimize
	assert(optimize(exp) == exp, "Test 1: optimize")
	assert(optimize(doubledExp) == squared, "Test 2: optimize")*/

}