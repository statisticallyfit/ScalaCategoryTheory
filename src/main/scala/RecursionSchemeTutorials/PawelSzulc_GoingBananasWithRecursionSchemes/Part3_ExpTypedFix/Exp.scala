package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part3_ExpTypedFix


import scalaz._
import slamdata.Predef._
import scalaz.Functor

import scala.language.higherKinds

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._


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


/**
 * Class Fix is a wrapper class, kills the recursion:
 *
 * `class Fix[F[_]](unFix: F[Fix[F]]`
 */


/*

object ExpThings {
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

object Exp {

}

object ExpRunner3 extends App {

	import org.scalactic._
	import TripleEquals._
	import Tolerance._


	val EPSILON: Double = 0.00001





	// stack two levels of Exp
	// NOTE: putting in the Fix into this expression to kill the recursion, infinte typing of Exp
	// NOTE: put in the types as per `class Fix[F[_]](unFix: F[Fix[F]]` so since unFix = IntValue(10)
	//  then the type is no longer Unit.
	// Decomposing the unFix: F[Fix[F]]
	//  1) F == IntValue
	//  2) [_] == Fix[Exp]
	//  THEREFORE: `unFix = IntValue[Fix[Exp]](10)`
	val exp1_FixAtLeaves = Sum(
		Fix(IntValue[Fix[Exp]](10)),
		Fix(IntValue[Fix[Exp]](5))
	)

	// NOTE: continuing to adjust the types
	val exp2_FixAtLeaves_FinishTypes: Exp[Fix[Exp]] = Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	)

	// unFix = Sum(Fix(IntValue(_), IntValue(_)))
	// 1) (outer) F == Sum
	// 2) [_] == Fix[Exp]
	// THEREFORE: unFix = Sum[Fix[Exp]](IntValue(_), IntValue(_))
	// ALSO: overall expr3 type is Fix[Exp] because the unfix is the Fix(Sum(_,_))
	val exp3_FixWrap = Fix( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	) )

	// No more infinite stacking of types!
	val exp4_FixWrap_FinishTypes: Fix[Exp] = Fix[Exp]( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	) )

	//TODO left off here, todo refactor the rest below same way as above



	//stack three levels of Exp
	val three_STACK: Exp[Exp[Exp[Unit]]] = Divide[Exp[Exp[Unit]]](
		DecValue[Exp[Unit]](5.2),
		Sum[Exp[Unit]](IntValue[Unit](10),IntValue[Unit](5) )
	)



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
}