package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part4_CataAnaAlgebra



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

	val optimize: Algebra[Exp, Fix[Exp]] = { // Exp[Fix[Exp]] => Fix[Exp]
		case Multiply(Fix(a1), Fix(a2)) if(a1 == a2) => Fix(Square(Fix(a1)))
		case other => Fix(other)
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


object TypeGetter {

	import scala.reflect.runtime.universe._

	def typeof[T: TypeTag](obj: T) = typeOf[T]


	def typeClean[T: TypeTag](obj: T): String = {

		val r1a = typeof(obj).toString.replace("Product with Serializable ", "")
		//val r1a = typeof(obj).toString.replace("Product with Serializable with ", "")
		val r1b = if(r1a.contains("[Product with Serializable]")) r1a else r1a.replace("Product with Serializable]", "]")
		val r2 = r1b.replace("_ >: ", "")
		val r3 = r2
			.replace("Object","")
			.replace("_ ", "")
			.replace(" <: ", "")
			.replace("<: ", "")
			.replace(" <:", "")
		val r4 = r3.replace("[with ", "[")
		val r5 = r4.replace("]with", "] with")

		val withAtFront: String = "with "

		val r6 = r5.substring(0, withAtFront.length) == withAtFront match {
			case true => r5.substring(withAtFront.length)
			case false => r5
		}

		val tempStr = r6


		val pkgs = tempStr
			.split("[\\[\\]]")
			.flatMap(e => e.split(' '))
			.filter(e => e.contains('.'))

		val oldNewReplacements = pkgs.distinct.map(p => (p, p.split('.').last))

		oldNewReplacements.foldLeft(tempStr)(
			(currentStrType, tuple) => tuple match {
				case (oldPkg: String, newShort: String) => currentStrType.replaceAll(oldPkg, newShort)
			}
		)
	}

}



object ExpRunner4 extends App {


	import ExpOps._
	import ExpFunctor._
	import TypeGetter._

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
	assert(doubleExp.cata(optimize).cata(mkString) == "((3 + 4))^2", "Test: doubleExp optimize string")
}