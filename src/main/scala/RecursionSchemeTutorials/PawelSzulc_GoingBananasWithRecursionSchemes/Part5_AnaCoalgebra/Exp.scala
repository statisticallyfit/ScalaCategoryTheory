package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra

/**
 * Tutorial source = https://www.slideshare.net/paulszulc/going-bananas-with-recursion-schemes-for-fixed-point-data-types-72006242?next_slideshow=1
 */

//import slamdata.Predef._
import scalaz.{Applicative, Functor}
import scalaz.syntax.all._

import scala.language.higherKinds
import matryoshka._
// NOTE: need this to avoid error of "No implicits found for Corecursive[T]" when doing anamorphism
// MEANING: Fix needs to implement BirecursiveT typeclass
import matryoshka.data.Fix.birecursiveT
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

	import scala.util.Try

	val mkString: Algebra[Exp, String] /*Exp[String] => String*/ = exp => exp match {
		case IntValue(v) => v.toString
		case DecValue(v) => v.toString
		case Sum(a1, a2) => s"($a1 + $a2)"
		case Multiply(a1, a2) => s"($a1 * $a2)"
		case Square(e) => {
			val tryInt: Option[Int] = Try(e.toInt).toOption
			val tryDouble: Option[Double] = Try(e.toDouble).toOption

			(tryInt, tryDouble) match {
				case (None, None) => s"($e)^2" // then not an int, more complicated still Exp type
				case _ => s"${tryInt.get}^2" // if either worked, then it is a number, so use no parentheses
			}
		}
		case Divide(a1, a2) => s"($a1 / $a2)"

	}

	// NOTE: redefining this function so that it evaluates nested Exps

	val optimize: Algebra[Exp, Fix[Exp]] = { // Exp[Fix[Exp]] => Fix[Exp]
		/*case Multiply(Fix(a1), Fix(a2)) if(a1 == a2) => Fix(Square(optimize(a1)))
		//case other => Fix(other) // this line alone without things below won't resolve nested squares.
		case Multiply(Fix(a1), Fix(a2)) => Fix(Multiply(optimize(a1), optimize(a2)))*/
		case Multiply(Fix(a1), Fix(a2)) => a1 == a2 match {
			case true => Fix(Square(optimize(a1)))
			case false => Fix(Multiply(optimize(a1), optimize(a2)))
		}
		case Sum(Fix(a1), Fix(a2)) => Fix(Sum(optimize(a1), optimize(a2)))
		case Divide(Fix(a1), Fix(a2)) => Fix(Divide(optimize(a1), optimize(a2)))
		case Square(Fix(a)) => Fix(Square(optimize(a)))
		case i@IntValue(_) => Fix(i)
		case d@DecValue(_) => Fix(d)
		//case other => optimize(other)
	}


	// NOTE: should rearrange (2 * (2 * (3 * (3 * 5)))) ===> ((((2 * 2) * 3) * 3) * 5)

	val rearrange: Algebra[Exp, Fix[Exp]] = { // Exp[Fix[Exp]] => Fix[Exp]
		/*case Multiply(Fix(a), Fix(Multiply(Fix(b), Fix(c)))) =>
			Fix(Multiply(
				Fix(Multiply(rearrange(a), rearrange(b))),
				rearrange(c)
			))*/

		case Multiply(Fix(IntValue(aInt)), Fix(Multiply(Fix(IntValue(bInt)), Fix(c)))) =>
			Fix(Multiply(
				Fix(Multiply(
					Fix(IntValue(aInt)), Fix(IntValue(bInt))
				)),
				rearrange(c)
			))
			// TODO why doesn't this work instead of the above one?
		/*case Multiply(Fix(IntValue(aInt)), Fix(Multiply(Fix(b), Fix(c)))) =>
			Fix(Multiply(
				Fix(Multiply(
					Fix(IntValue(aInt)), rearrange(b)
				)),
				rearrange(c)
			))*/

		case Multiply(Fix(a1), Fix(a2)) =>
			Fix(Multiply(rearrange(a1), rearrange(a2)))

		case Sum(Fix(a1), Fix(a2)) =>
			Fix(Sum(rearrange(a1), rearrange(a2)))

		case Divide(Fix(a1), Fix(a2)) =>
			Fix(Divide(rearrange(a1), rearrange(a2)))

		case Square(Fix(a)) => Fix(Square(rearrange(a)))

		case i@IntValue(_) => Fix(i)

		case d@DecValue(_) => Fix(d)
	}


	// PROP: `type Coalgebra[F[_], A] = A => F[A]`

	// TODO why here Int => Exp[Int] with no Fix constructor? Does this have something to do with Coalgebra vs.  Algebra?
	val divisors: Coalgebra[Exp, Int] = { // Int => Exp[Int]
		case n if(n % 2 == 0 && n != 2) => Multiply(2, n / 2)
		case n if(n % 3 == 0 && n != 3) => Multiply(3, n / 3)
		case n if(n % 5 == 0 && n != 5) => Multiply(5, n / 5)
		case n => IntValue(n)
	}
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







object ExpRunner5 extends App {


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

	assert(nestedDoubleExp.cata(optimize).cata(mkString) == "((5^2 + 4))^2" &&
		optimize(nestedDoubleExp.unFix).cata(mkString) == "((5^2 + 4))^2" &&
		nestedSquareExp.cata(optimize).cata(mkString) == "((5^2 + 4))^2",
		"Test: nestedDouble optimize == nested squared optimize")







	// PROPERTY: `def ana[A](a: A)(f: Coalgebra[Base, A])(implicit BF: Functor[Base]): T`

	// TODO figure out why need Fix[Exp] here as type parameter and how it is related to the error
	// "No implicits found for type parameter T: Corecursive.Aux[Exp[Int], Int]"

	// TESTING
	val divisorsOf12: Fix[Exp] = Fix(
		Multiply[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](2)),
			Fix(Multiply[Fix[Exp]](
				Fix(IntValue[Fix[Exp]](2)),
				Fix(IntValue[Fix[Exp]](3))
			))
		)
	)

	assert(12.ana[Fix[Exp]](divisors) == divisorsOf12, "Test: anamorphism 12")
	assert(12.ana[Fix[Exp]](divisors).cata(mkString) == "(2 * (2 * 3))", "Test: divisors of 12 (string)")


	val divisorsOf28: Fix[Exp] = Fix(Multiply(
		Fix(
			IntValue(2)),
		Fix(Multiply(
			Fix(IntValue(2)),
			Fix(Multiply(
				Fix(IntValue(3)),
				Fix(Multiply(
					Fix(IntValue(3)),
					Fix(IntValue(5))
				))
			))
		))
	))
	val e = Multiply(Fix(IntValue[Fix[Exp]](5)), Fix(IntValue[Fix[Exp]](4))).embed

	assert(180.ana[Fix[Exp]](divisors) == divisorsOf28,
		"Test: anamorphism 180")

	assert(180.ana[Fix[Exp]](divisors).cata(mkString) == "(2 * (2 * (3 * (3 * 5))))",
		"Test: anamorphism 180 (string)")
	assert(180.ana[Fix[Exp]](divisors).cata(rearrange).cata(mkString) == "((2 * 2) * ((3 * 3) * 5))",
		"Test: ana . cata 180 rearrange divisors string")
	assert(180.ana[Fix[Exp]](divisors)
		.cata(rearrange)
		.cata(optimize)
		.cata(mkString) == "(2^2 * (3^2 * 5))",
		"Test: ana . cata 180 rearrange optimize divisors string")

	assert(32.ana[Fix[Exp]](divisors).cata(evaluate) == 32.0, "Test: cata . ana 32 evaluate")

	/*assert(16.ana[Fix[Exp]](divisors).cata(mkString) == "(2 * (2 * (2 * 2)))", "Test: ana (16) string")
	/*assert(16.ana[Fix[Exp]](divisors).cata(rearrange).cata(mkString) == "(2 * (2 * (2 * 2)))",
		"Test: ana (16) string")*/
	// TODO fix square for rearrange: ((2 * 2^2) * 2) -----> 2^4
	// TODO consider using Pow? instead of square? then adapting the right branch of the multiply in the rearrange  method?
	println(16.ana[Fix[Exp]](divisors).cata(rearrange).cata(mkString))
	println(16.ana[Fix[Exp]](divisors)
		.cata(rearrange)
		.cata(optimize)
		.cata(mkString))*/



}
