package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part4_CataAlgebra



/*import cats._
import cats.Functor
import cats.implicits._

import higherkindness.droste.Algebra
import higherkindness.droste.data._
import higherkindness.droste.implicits._*/


import slamdata.Predef._

import scalaz.{Functor, Applicative}
import scalaz.syntax.all._

import scala.language.higherKinds

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._


sealed trait Exp[A]

final case class IntValue[A](v: Int) extends Exp[A]
final case class DecValue[A](v: Double) extends Exp[A]
final case class Sum[A](exp1: A, exp2: A) extends Exp[A]
final case class Multiply[A](exp1: A, exp2: A) extends Exp[A]
final case class Divide[A](exp1: A, exp2: A) extends Exp[A]
final case class Square[A](exp: A) extends Exp[A]


object ExpOps {

	// PROP: `type Algebra[F[_], A] = F[A] => A`

	val evaluate: Algebra[Exp, Double]/*Exp[uble] => Double*/ = exp => exp match {
		case IntValue(v) => v.toDouble
		case DecValue(v) => v
		case Sum(a1, a2) => a1 + a2 //evaluate(a1) + evaluate(a2)
		case Multiply(a1, a2) => a1 * a2 // evaluate(a1) * evaluate(a2)
		case Square(e) => e * e
		case Divide(a1, a2) => a1 / a2 // evaluate(a1) / evaluate(a2)
	}

	val mkString: Algebra[Exp, String] /*Exp[A] => String*/ = exp => exp match {
		case IntValue(v) => v.toString
		case DecValue(v) => v.toString
		case Sum(a1, a2) => s"($a1 + $a2)"
		case Multiply(a1, a2) => s"($a1 * $a2)"
		case Square(e) => s"($e)^2"
		case Divide(a1, a2) => s"($a1 / $a2)"

	}

	// NOTE: redefining this function so that it evaluates nested Exps

	val optimize: Algebra[Exp, Fix[Exp]] = { // Exp[Fix[Exp]] => Fix[Exp]
		case Multiply(Fix(a1), Fix(a2)) if(a1 == a2) => Fix(Square(optimize(a1)))
		//case other => Fix(other) // this line alone without things below won't resolve nested squares.
		case Sum(Fix(a1), Fix(a2)) => Fix(Sum(optimize(a1), optimize(a2)))
		case Divide(Fix(a1), Fix(a2)) => Fix(Divide(optimize(a1), optimize(a2)))
		case Square(Fix(a)) => Fix(Square(optimize(a)))
		case i@IntValue(_) => Fix(i)
		case d@DecValue(_) => Fix(d)
		//case other => optimize(other)
	}
	/*Exp[A] => Exp[A] = exp => exp match {
		case Multiply(a1, a2) if(a1 == a2) => Square(optimize(a1))
		case Multiply(a1, a2) => Multiply(optimize(a1), optimize(a2))
		case IntValue(v) => IntValue(v)
		case DecValue(v) => DecValue(v)
		case Sum(a1, a2) => Sum(optimize(a1), optimize(a2))
		case Square(e) => Square(optimize(e))
		case Divide(a1, a2) => Divide(optimize(a1), optimize(a2))
	}*/
}




object ExpFunctor  {

	implicit def expFunctor: Functor[Exp] = new Functor[Exp] {
		def map[A, B](fa: Exp[A])(f: A => B): Exp[B] = fa match {
			case IntValue(v) => IntValue(v)
			case DecValue(v) => DecValue(v)
			case Sum(a1, a2) => Sum(f(a1), f(a2))
			case Multiply(a1, a2) => Multiply(f(a1), f(a2))
			case Divide(a1, a2) => Divide(f(a1), f(a2))
			case Square(a) => Square(f(a))
		}
	}
}




object ExpRunner4 extends App {


	import ExpOps._
	import ExpFunctor._
	import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.util.TypeGetter

	import org.scalactic._
	import TripleEquals._
	import Tolerance._

	val EPSILON: Double = 0.00001


	// PROP: `def cata[A](f: Algebra[F, A])(implicit BF: Functor[F])`

	// NOTE

	val expDiv: Fix[Exp] = Fix[Exp](Divide[Fix[Exp]](
		Fix[Exp](DecValue[Fix[Exp]](5.2)),
		Fix[Exp](Sum[Fix[Exp]](
			Fix[Exp](IntValue[Fix[Exp]](10)),
			Fix[Exp](DecValue[Fix[Exp]](5))
		))
	))


	// TESTING
	assert(TypeGetter.typeClean(expDiv) == "Fix[Exp]", "Test: expDiv type")
	//assert( (evaluate(exp) - 4.3333333) < EPSILON, "Test: evaluate")
	assert(expDiv.cata(evaluate) === 0.346666666667 +- EPSILON,
		"Test: expDiv.cata(evaluate)")

	assert(expDiv.cata(mkString) == "(5.2 / (10 + 5.0))", "Test: expDiv.cata(mkString)")



	// NOTE
	val expMult = Fix[Exp](Multiply[Fix[Exp]]( //
		Fix[Exp](Sum(
			Fix[Exp](IntValue[Fix[Exp]](10)),
			Fix[Exp](DecValue[Fix[Exp]](2.5))
		)),
		Fix[Exp](Divide[Fix[Exp]](
			Fix[Exp](DecValue[Fix[Exp]](5.2)),
			Fix[Exp](Sum[Fix[Exp]](
				Fix[Exp](IntValue[Fix[Exp]](10)),
				Fix[Exp](IntValue[Fix[Exp]](5))
			))
		))
	))
	// TESTING
	assert(TypeGetter.typeClean(expMult) == "Fix[Exp]", "Test: expMult type")

	assert(expMult.cata(evaluate) === 4.3333333333333 +- EPSILON,
		"Test: expMult.cata(evaluate)")

	assert(expMult.cata(mkString) == "((10 + 2.5) * (5.2 / (10 + 5)))",
		"Test: expMult.cata(mkString)")



	// NOTE

	val doubleExp = Fix[Exp](Multiply(
		Fix[Exp](Sum[Fix[Exp]](
			Fix[Exp](IntValue[Fix[Exp]](3)),
			Fix[Exp](IntValue[Fix[Exp]](4))
		)),
		Fix[Exp](Sum[Fix[Exp]](
			Fix[Exp](IntValue[Fix[Exp]](3)),
			Fix[Exp](IntValue[Fix[Exp]](4))
		))
	))
	val squaredExp = Fix[Exp](Square[Fix[Exp]](
		Fix[Exp](Sum[Fix[Exp]](
			Fix[Exp](IntValue[Fix[Exp]](3)),
			Fix[Exp](IntValue[Fix[Exp]](4))
		))
	))
	// TESTING
	assert(TypeGetter.typeClean(squaredExp) == "Fix[Exp]", "Test: squared type")
	assert(TypeGetter.typeClean(doubleExp) == "Fix[Exp]", "Test: doubled type")

	assert(squaredExp.cata(evaluate) == doubleExp.cata(evaluate) &&
		squaredExp.cata(evaluate) == 49.0, "Test: squared.cata(evaluate)")

	assert(squaredExp.cata(mkString) == "((3 + 4))^2", "Test: squared.cata(mkString)")
	assert(doubleExp.cata(mkString) == "((3 + 4) * (3 + 4))", "Test: doubleExp.cata(mkString)")


	assert(doubleExp.cata(optimize) == squaredExp, "Test: doubleExp.cata(optimize) == squaredExp")
	assert(squaredExp.cata(optimize) == squaredExp, "Test: squaredExp.cata(optimize) == squaredExp")

	assert(doubleExp.cata(optimize).cata(mkString) == "((3 + 4))^2" &&
		squaredExp.cata(mkString) == doubleExp.cata(optimize).cata(mkString),
		"Test: doubleExp optimize string")




	// NOTE
	val nestedDoubleExp = Fix(Multiply[Fix[Exp]](
		Fix(Sum[Fix[Exp]](
			Fix(Multiply[Fix[Exp]](
				Fix(IntValue[Fix[Exp]](5)),
				Fix(IntValue[Fix[Exp]](5))
			)),
			Fix(IntValue[Fix[Exp]](4))
		)),
		Fix(Sum[Fix[Exp]](
			Fix(Multiply[Fix[Exp]](
				Fix(IntValue[Fix[Exp]](5)),
				Fix(IntValue[Fix[Exp]](5))
			)),
			Fix(IntValue[Fix[Exp]](4))
		))
	))
	val nestedSquareExp = Fix(Square[Fix[Exp]](
		Fix(Sum[Fix[Exp]](
			Fix(Multiply[Fix[Exp]](
				Fix(IntValue[Fix[Exp]](5)),
				Fix(IntValue[Fix[Exp]](5))
			)),
			Fix(IntValue[Fix[Exp]](4))
		))
	))
	// TESTING
	assert(nestedDoubleExp.cata(evaluate) == 841.0 &&
		nestedDoubleExp.cata(evaluate) == nestedSquareExp.cata(evaluate),
		"Test: nestedDoubleExp.cata(evaluate)")


	assert(nestedDoubleExp.cata(mkString) == "(((5 * 5) + 4) * ((5 * 5) + 4))",
		"Test: nestedDoubleExp.cata(mkString)")
	assert(nestedSquareExp.cata(mkString) == "(((5 * 5) + 4))^2",
		"Test: nestedSquareExp.cata(mkString)")

	assert(nestedDoubleExp.cata(optimize).cata(mkString) == "(((5)^2 + 4))^2" &&
		optimize(nestedDoubleExp.unFix).cata(mkString) == "(((5)^2 + 4))^2" &&
		nestedSquareExp.cata(optimize).cata(mkString) == "(((5)^2 + 4))^2",
		"Test: nestedDouble optimize == nested squared optimize")


}