package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part3_ExpTypedFix




import slamdata.Predef._
import scalaz._
//import scalaz.{Functor, Applicative}
//import scalaz.syntax.all._

import scala.language.higherKinds

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._


import cats._
import cats.implicits._

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

	/*implicit def expMonoid[A](implicit ev: cats.Monoid[A]): cats.Monoid[Exp[A]] = new cats.Monoid[Exp[A]] {
		def empty: Exp[A] = IntValue[A](0) // TODO int 0 or multip 1? (sum or addition monoid?)

		def combine(x: Exp[A], y: Exp[A]): Exp[A] = (x, y) match {
			case (Sum(x1, x2), Sum(y1, y2)) => Sum(ev.combine(x1, x2), ev.combine(y1, y2))
			case (IntValue(v1), IntValue(v2)) => IntValue(v1 + v2) //TODO
			case (IntValue(v1), IntValue(v2)) => IntValue(v1 + v2) //TODO
		}
	}*/

	implicit def expFunctor: cats.Functor[Exp] = new cats.Functor[Exp] {
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

	def getType[T: TypeTag](obj: T) = {
		def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

		getTypeTag(1).tpe
	}


	def typeof[T: TypeTag](obj: T) = typeOf[T]

	def cleanName[T: TypeTag](obj: T): String = {

		val topLevelName: String = obj.getClass.toString.split(' ')(1)
		// NOTE:
		// CASE 1: If obj is part of package, then getName returns its entire package path (So passing
		// Fix(Sum(intval, intval)) would result in "matryoshka.data.Fix"
		// CASE 2: If obj is of a user-defined class (not pkg), then getName returns its exact top-level
		// name. (So passing Sum(intval, intval) would result in "Sum" )

		//If CASE 2, then we are done, just return the name (there are no package dots to eliminate)
		///if (!topLevelName.contains('.')){

		if (!typeof(obj).toString.contains('.')) {
			return topLevelName
		}

		// HELP: when obj == Sum(intval, intval), then toplevelname CONTAINS the "." even though it
		//  doesn't!!!!  WHY???


		//Else, contain to clean away the package name of upper-level packages:
		val notUsed = topLevelName.split('.').reverse.tail.reverse.mkString(".")

		val notUsedWithDot: String = notUsed + "."

		typeof(obj).toString.replace(notUsedWithDot, "")

	}

	val vf = Fix[Exp]( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	) )
	val v = Sum(IntValue(2), IntValue(3))
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
	val expSum1_FixAtLeaves = Sum(
		Fix(IntValue[Fix[Exp]](10)),
		Fix(IntValue[Fix[Exp]](5))
	)

	// NOTE: continuing to adjust the types
	val expSum2_FixAtLeaves_FinishTypes: Exp[Fix[Exp]] = Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	)

	// unFix = Sum(Fix(IntValue(_), IntValue(_)))
	// 1) (outer) F == Sum
	// 2) [_] == Fix[Exp]
	// THEREFORE: unFix = Sum[Fix[Exp]](IntValue(_), IntValue(_))
	// ALSO: overall expr3 type is Fix[Exp] because the unfix is the Fix(Sum(_,_))
	val expSum3_FixWrap = Fix( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	) )

	// No more infinite stacking of types!
	val expSum4_FixWrap_FinishTypes: Fix[Exp] = Fix[Exp]( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	) )

	//TODO left off here, todo refactor the rest below same way as above



	// ------------------------------------------------------------------------------------------------

	// NOTE: the expression BEFORE Fixing it

	// PROPERTY: `class Fix[F[_]](unFix: F[Fix[F]]`

	/*val three_STACK: Exp[Exp[Exp[Unit]]] = Divide[Exp[Exp[Unit]]](
		DecValue[Exp[Unit]](5.2),
		Sum[Exp[Unit]](IntValue[Unit](10),IntValue[Unit](5) )
	)*/

	// 1) F == IntValue
	// 2) Fix[F] == Fix[Exp]
	val expDiv1_FixLeaves = Divide(
		Fix(DecValue[Fix[Exp]](5.2)),
		Sum(Fix(IntValue[Fix[Exp]](10)), Fix(IntValue[Fix[Exp]](5)))
	)

	// 1)
	val expDiv2_FixLeaves_FinishTypes: Exp[Fix[Exp]] = Divide(
		DecValue(2.5), //Fix(DecValue[Fix[Exp]](5.2)),
		Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(IntValue[Fix[Exp]](5))
		)
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