package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part4_CataAnaAlgebra




import slamdata.Predef._

import scalaz._
import scalaz.{Functor, Applicative}
import scalaz.syntax.all._

import scala.language.higherKinds

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._


/*import cats._
import cats.Functor
import cats.implicits._

import higherkindness.droste.Algebra
import higherkindness.droste.data._
import higherkindness.droste.implicits._*/



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

	/*val optimize/*Exp[A] => Exp[A]*/ = exp => exp match {
		case Multiply(a1, a2) if(a1 == a2) => Square(optimize(a1))
		case Multiply(a1, a2) => Multiply(optimize(a1), optimize(a2))
		case IntValue(v) => IntValue(v)
		case DecValue(v) => DecValue(v)
		case Sum(a1, a2) => Sum(optimize(a1), optimize(a2))
		case Square(e) => Square(optimize(e))
		case Divide(a1, a2) => Divide(optimize(a1), optimize(a2))
	}*/
}







object Exp {

	/*implicit def expMonoid[A](implicit ev: cats.Monoid[A]): cats.Monoid[Exp[A]] = new cats.Monoid[Exp[A]] {
		def empty: Exp[A] = IntValue[A](0) // TODO int 0 or multip 1? (sum or addition monoid?)

		def combine(x: Exp[A], y: Exp[A]): Exp[A] = (x, y) match {
			case (Sum(x1, x2), Sum(y1, y2)) => Sum(ev.combine(x1, x2), ev.combine(y1, y2))
			case (IntValue(v1), IntValue(v2)) => IntValue(v1 + v2) //TODO
			case (IntValue(v1), IntValue(v2)) => IntValue(v1 + v2) //TODO
		}
	}*/

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

	//TODO how to define ap so that can return Exp[B] not Exp[Exp[B]]?
	/*implicit def expApply: cats.Apply[Exp] = new cats.Apply[Exp] {
		//def pure[A](x: A): Exp[A] = Divide (Square(x), x)

		def ap[A, B](ff: Exp[A => B])(fa: Exp[A]): Exp[B] = {
			fa.map(a => ff.map(f => f(a)))
		}
			//fa.flatMap(a => ff.map(f: (A => B) => f(a)))

		def map[A, B](fa: Exp[A])(f: A => B): Exp[B] = fa.map(f) // need to copy from Functor

		//def product[A, B](fa: Exp[A], fb: Exp[B]): Exp[(A, B)]

	}*/


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

	import org.scalactic._
	import TripleEquals._
	import Tolerance._

	import ExpOps._
	import Exp._
	import TypeGetter._


	// PROP: `def cata[A](f: Algebra[F, A])(implicit BF: Functor[F])`


	val EPSILON: Double = 0.00001

	val expDiv: Fix[Exp] = Fix(Divide(
		Fix(DecValue(5.2)),
		Fix(Sum(
			Fix(IntValue(10)),
			Fix(DecValue(5))
		))
	))

	
	// TESTING
	//assert( (evaluate(exp) - 4.3333333) < EPSILON, "Test: evaluate")
	assert(expDiv.cata(evaluate) === 0.346666666667 +- EPSILON,
		"Test: expDiv.cata(evaluate)")



	val expMult5_FixWrap = Fix(Multiply[Fix[Exp]]( //
		Fix(Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](2.5))
		)),
		Fix(Divide[Fix[Exp]](
			Fix(DecValue[Fix[Exp]](5.2)),
			Fix(Sum[Fix[Exp]](
				Fix(IntValue[Fix[Exp]](10)),
				Fix(IntValue[Fix[Exp]](5))
			))
		))
	))
}