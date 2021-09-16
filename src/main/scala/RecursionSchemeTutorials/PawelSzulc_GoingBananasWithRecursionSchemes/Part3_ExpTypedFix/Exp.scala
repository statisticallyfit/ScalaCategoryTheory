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

	/*def cleanName[T: TypeTag](obj: T): String = {

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
		val notUsed = topLevelName.split('.').toList.reverse.tail.reverse.mkString(".")

		val notUsedWithDot: String = notUsed + "."

		typeof(obj).toString.replace(notUsedWithDot, "")

	}*/

	//TODO apply this new version to all below tests to see if they still work
	def mix[T: TypeTag](obj: T): String = {

		val r1a = typeof(obj).toString.replace("Product with Serializable ", "")
		val r1b = if(r1a.contains("[Product with Serializable]")) r1a else r1a.replace("Product with Serializable]", "]")
		val r2 = r1b.replace("_ >: ", "")
		val r3 = r2.replace("[with ", "[")
		val r4 = r3.replace("Object","").replace("_ ", "").replace(" <: ", "").replace("<: ", "").replace(" <:", "")
		val r5 = r4.replace("]with", "] with")

		val tempStr = r5


		val pkgs = tempStr.split("[\\[\\]]").flatMap(e => e.split(' ')).filter(e => e.contains('.'))

		val oldNewReplacements = pkgs.distinct.map(p => (p, p.split('.').last))

		oldNewReplacements.foldLeft(tempStr)(
			(currentStrType, tuple) => tuple match {
				case (oldPkg: String, newShort: String) => currentStrType.replaceAll(oldPkg, newShort)
			}
		)
	}

	def getTypeName[T: TypeTag](obj: T): String = {

		val r1 = typeof(obj).toString
			.replace("Product with Serializable ", "")
			.replace("Product with Serializable]", "]")
		val r2 = r1.replace("_ >: ", "")
		val r3 = r2.replace("[with ", "[")
		val r4 = r3
			.replace("Object","")
			.replace("_ ", "")
			.replace(" <: ", "")
			.replace("<: ", "")
			.replace(" <:", "")

		val pkgs = r4.split("[\\[\\]]").flatMap(e => e.split(' ')).filter(e => e.contains('.'))

		val oldNewReplacements = pkgs.distinct.map(p => (p, p.split('.').last))

		oldNewReplacements.foldLeft(r4)(
			(currentStrType, tuple) => tuple match {
				case (pkgName: String, className: String) =>
					currentStrType.replaceAll(pkgName, className)
			}
		)
	}

	val vf = Fix[Exp]( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](IntValue[Fix[Exp]](5))
	) )
	val v = Sum(IntValue(2), IntValue(3))
}


object ExpRunner3 extends App {


	//NOTE: expr before applying Fix

	val expSum_Infinite_NoType = Sum(IntValue(10), DecValue(5))

	val expSum_Infinite_Typed: Exp[Exp[Unit]] = Sum[Exp[Unit]](
		IntValue[Unit](10),
		DecValue[Unit](5)
	)
	// TESTING
	assert(TypeGetter.getTypeName(expSum_Infinite_NoType) == "Sum[Exp[Nothing]]",
		"Test 1a: BEFORE Fix: expSum_Infinite_NoType")
	// TESTI
	assert(TypeGetter.getTypeName(expSum_Infinite_Typed) == "Exp[Exp[Unit]]",
		"Test 1b: BEFORE Fix: expSum_Infinite_Typed")


	// NOTE: Applying Fix now to get rid of Exp[Exp[Exp....]]] recursion

	// PROPERTY: `class Fix[F[_]](unFix: F[Fix[F]]`

	// Putting in the Fix into this expression to kill the recursion, infinte typing of Exp
	// Put in the types as per `class Fix[F[_]](unFix: F[Fix[F]]` so since unFix = IntValue(10)
	//  then the type is no longer Unit.
	// Decomposing the unFix: F[Fix[F]]
	//  1) F == IntValue
	//  2) [_] == Fix[Exp]
	//  THEREFORE: `unFix = IntValue[Fix[Exp]](10)`
	val expSum1_FixLeaves = Sum(
		Fix(IntValue[Fix[Exp]](10)),
		Fix(DecValue[Fix[Exp]](5))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expSum1_FixLeaves) == "Sum[Fix[Exp]]",
		"Test 1a: DURING Fix: expSum1_FixLeaves")

	val expSum2_FixLeaves_AddTypes = Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](DecValue[Fix[Exp]](5))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expSum2_FixLeaves_AddTypes) == "Sum[Fix[Exp]]",
		"Test 1b: DURING Fix: expSum2_FixLeaves_AddTypes")

	val expSum2_FixLeaves_FinishTypes: Exp[Fix[Exp]] = Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](DecValue[Fix[Exp]](5))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expSum2_FixLeaves_FinishTypes) == "Exp[Fix[Exp]]",
		"Test 1c: DURING Fix: expSum2_FixLeaves_FinishTypes")

	// unFix = Sum(Fix(IntValue(_), IntValue(_)))
	// 1) (outer) F == Sum
	// 2) [_] == Fix[Exp]
	// THEREFORE: unFix = Sum[Fix[Exp]](IntValue(_), IntValue(_))
	// ALSO: overall expr3 type is Fix[Exp] because the unfix is the Fix(Sum(_,_))
	val expSum3_FixWrap = Fix( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](DecValue[Fix[Exp]](5))
	) )
	// TESTING
	assert(TypeGetter.getTypeName(expSum3_FixWrap) == "Fix[Exp]",
		"Test 1d: DURING Fix: expSum3_FixWrap")

	// No more infinite stacking of types!
	val expSum4_FixWrap_FinishTypes: Fix[Exp] = Fix[Exp]( Sum[Fix[Exp]](
		Fix[Exp](IntValue[Fix[Exp]](10)),
		Fix[Exp](DecValue[Fix[Exp]](5))
	) )
	// TESTING
	assert(TypeGetter.getTypeName(expSum4_FixWrap_FinishTypes) == "Fix[Exp]",
		"Test 1e: END Fix: expSum4_FixWrap_FinishTypes")


	// ------------------------------------------------------------------------------------------------


	// NOTE: the expression BEFORE Fixing it

	val expDiv_Infinite_NoType = Divide(
		DecValue(5.2),
		Sum(IntValue(10), DecValue(5.0))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv_Infinite_NoType) == "Divide[Exp[Exp[Nothing]]]",
		"Test 2a: BEFORE Fix: expDiv_Infinite_NoType")

	val expDiv_Infinite_Typed: Exp[Exp[Exp[Unit]]] = Divide[Exp[Exp[Unit]]](
		DecValue[Exp[Unit]](5.2),
		Sum[Exp[Unit]](IntValue[Unit](10),DecValue[Unit](5) )
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv_Infinite_Typed) == "Exp[Exp[Exp[Unit]]]",
		"Test 2a: BEFORE Fix: expDiv_Infinite_Typed")

	// NOTE: starting to wrap the values in Fix, starting at the leaves

	// PROPERTY: `class Fix[F[_]](unFix: F[Fix[F]]`

	// 1) F == IntValue
	// 2) Fix[F] == Fix[Exp]
	// NOTE: throws error if you define this without the Fix[Exp] type for the leaves
	val expDiv1_FixLeaves = Divide(
		DecValue[Exp[Unit]](5.2),
		Sum(
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](5))
		)
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv1_FixLeaves) == "Divide[Exp[Fix[Exp] with Exp[Unit]]]",
		"Test 2a: DURING Fix: expDiv1_FixLeaves")

	val expDiv2_FixLeaves_FinishTypes/*: Exp[Fix[Exp]]*/ = Divide(
		DecValue[Exp[Unit]](5.2),
		Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](5))
		)
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv2_FixLeaves_FinishTypes) == "Divide[Exp[Fix[Exp] with Exp[Unit]]]",
		"Test 2b: DURING Fix: expDiv2_FixLeaves_FinishTypes")

	val expDiv3_FixMiddle_part1/*: Exp[Fix[Exp]]*/ = Divide(
		DecValue[Exp[Unit]](5.2),
		Fix(Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](5))
		))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv3_FixMiddle_part1) == "Divide[Product with Serializable]",
		"Test 2c: DURING Fix: expDiv3_FixMiddle_part1")

	val expDiv3_FixMiddle_part2/*: Exp[Fix[Exp]]*/ = Divide(
		Fix(DecValue[Fix[Exp]](5.2)),
		Fix(Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](5))
		))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv3_FixMiddle_part2) == "Divide[Fix[Exp]]",
		"Test 2d: DURING Fix: expDiv3_FixMiddle_part2")

	val expDiv4_FixMiddle_FinishTypes/*: Exp[Fix[Exp]]*/ = Divide[Fix[Exp]](
		Fix(DecValue[Fix[Exp]](5.2)),
		Fix(Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](5))
		))
	)
	// TESTING
	assert(TypeGetter.getTypeName(expDiv4_FixMiddle_FinishTypes) == "Divide[Fix[Exp]]",
		"Test 2e: DURING Fix: expDiv4_FixMiddle_FinishTypes")


	val expDiv5_FixWrap/*: Fix[Exp]*/ = Fix(Divide[Fix[Exp]](
		Fix(DecValue[Fix[Exp]](5.2)),
		Fix(Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](5))
		))
	))
	// TESTING
	assert(TypeGetter.getTypeName(expDiv5_FixWrap) == "Fix[Exp]",
		"Test 2f: END Fix: expDiv5_FixWrap")



	// ------------------------------------------------------------------------------------------------

	// NOTE: the expression BEFORE Fixing it
	// non-typed
	val expMult_Infinite_NoType = Multiply(
		Sum(
			IntValue(10),
			DecValue(2.5)
		),
		Divide(
			DecValue(5.2),
			Sum(
				IntValue(10),
				IntValue(5))
		)
	)
	// TODO left off here apply the new definition of getType to all the above to see if they still work
	// typed
	val expMult_Infinite_Typed: Exp[Exp[Exp[Exp[Unit]]]] = Multiply[Exp[Exp[Exp[Unit]]]]( //
		Sum[Exp[Exp[Unit]]](
			IntValue[Exp[Unit]](10),
			DecValue[Exp[Unit]](2.5)
		),
		Divide[Exp[Exp[Unit]]](
			DecValue[Exp[Unit]](5.2),
			Sum[Exp[Unit]](
				IntValue[Unit](10),
				IntValue[Unit](5)
			)
		)
	)

	// PROPERTY: `class Fix[F[_]](unFix: F[Fix[F]]`

	val expMult1_FixLeaves  = Multiply( //
		Sum[Fix[Exp]](
			Fix(IntValue[Fix[Exp]](10)),
			Fix(DecValue[Fix[Exp]](2.5))
		),
		Divide(
			DecValue[Exp[Unit]](5.2),
			Sum[Fix[Exp]](
				Fix(IntValue[Fix[Exp]](10)),
				Fix(IntValue[Fix[Exp]](5))
			)
		)
	)
	println(TypeGetter.cleanName(expMult1_FixLeaves.exp1))
	println(TypeGetter.typeof(expMult1_FixLeaves.exp1))

	val expMult_end  = Fix[Exp](Multiply[Fix[Exp]]( //
		Fix[Exp](Sum[Fix[Exp]](
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


	// ------------------------------------------------------------------------------------------------

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