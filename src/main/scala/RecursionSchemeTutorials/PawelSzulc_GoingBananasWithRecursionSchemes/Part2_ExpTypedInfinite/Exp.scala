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


}