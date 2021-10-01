package RecursionSchemeTutorials.BeniVF_PracticalDroste


import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.data.Fix
//import higherkindness.droste.syntax.FixSyntax._
import higherkindness.droste.syntax.all._
//import higherkindness.droste.syntax._



//import Basis._ // must import drostebasisforfix (implicit)

import cats._
import cats.implicits._
//import cats.syntax.all._

/**
 * Tutorial source = https://github.com/BeniVF/practical-droste/blob/master/src/main/scala/list.scala#L20-L21
 */


sealed trait ListF[A, B]

object ListF{

	// The data structure
	final case class ConsF[A, B](head: A, tail: B) extends ListF[A, B]
	final case class NilF[A, B]() extends ListF[A, B]

	// Fixed points (to kill the recursion)
	type FixList[A] = Fix[ListF[A, ?]]

	object FixList {

		def wrap[A](a: A): FixList[A]                    = {
			// TODO source code uses .fix - cannot get this loaded?
			Fix[ListF[A, ?]](ConsF(a, Fix[ListF[A, ?]](NilF())))
		}
		def cons[A](head: A, tail: FixList[A]): FixList[A] = {
			Fix[ListF[A, ?]](ConsF(head, tail))
		}
	}
	//Functor
	// :kind ListF[A, B] is * -> * -> * so need to keep type A const to fit in with Functor's kind * -> *
	implicit def listFFunctor[A]: Functor[ListF[A, ?]] = new Functor[ListF[A, ?]] {
		def map[B, C](fa: ListF[A, B])(f: B => C): ListF[A, C] = fa match {
			case NilF()            => NilF()
			case ConsF(head, tail) => ConsF(head, f(tail))
		}
	}

	/**
	 * Catamorphism operations
	 *
	 * PROP: `type Algebra[F[_], B] = F[B] => B`
	 *
	 * PROP: `def cata[F[_]: Functor, R, B](algebra: Algebra[F, B])(implicit project: Project[F, R]): R => B`
	 */
	// ----------------------------------------------------------------------------------------------


	def algebraToList[A]: Algebra[ListF[A, ?], List[A]] = { // ListF[A, List[A]] => List[A]
		Algebra {
			case NilF() => Nil
			case ConsF(head : A, tail : List[A]) => head :: tail
		}
		// Algebra[F, B]: F[B] => B
		// cata[F, R, B]
		// ---> F[_]: ListF[A, ?]
		// ---> B := List[A]
	}


	// NOTE: need the implicit Basis because otherwise ERROR: no implicits for Project[F, R]
	// TODO changed this to be Project not Basis
	def toList[A, B](r: B)(implicit ev: Project[ListF[A, ?], B]): List[A] = {
		scheme.cata[ListF[A, ?], B, List[A]](algebraToList[A]).apply(r)
		// Algebra[F, B]: F[B] => B
		// Project[F, R],   Basis[F, R]
		// cata[F, R, B].apply: R => B
		// ---> F[_]: ListF[A, ?]
		// ---> R := B
		// ---> B := List[A]
	}

	// ----------------------------------------------------------------------------------------------
	def algebraLength[A]: Algebra[ListF[A, ?], Int] = // ListF[A, Int] => Int
		Algebra {
			case NilF() => 0
			case ConsF(head : A, accLen: Int) => 1 + accLen
		}


	def algebraShow[A: Show]: Algebra[ListF[A, ?], String] = // ListF[A, String] => String
		Algebra {
			case NilF() => "."
			case ConsF(head: A, accStr: String) => s"${head.show} :: $accStr"
		}

	def algebraMonoid[A: Monoid]: Algebra[ListF[A, ?], A] = // ListF[A, A] => A
		Algebra {
			case NilF() => Monoid[A].empty
			case ConsF(head: A, tail: A) => Monoid[A].combine(head, tail)
		}


	// TODO changed this to be Project instead of Basis

	def combineAndShowAlgebras[A: Show : Monoid, B](r: B)(implicit ev: Project[ListF[A, ?], B]): (A, String) = {
		scheme.cata[ListF[A, ?], B, (A, String)](algebraMonoid[A].zip(algebraShow[A])).apply(r)
		// Algebra[F, B]: F[B] => B
		// Project[F, R],   Basis[F, R]
		// cata[F, R, B].apply: R => B
		// ---> F[_]: ListF[A, ?]
		// ---> R := B
		// ---> B := (A, String)
	}

	// ----------------------------------------------------------------------------------------------

	/**
	 * Anamorphism operations
	 *
	 * PROP: `type Coalgebra[F[_], A] = A => F[A]`
	 *
	 * PROP: `def ana[F[_]: Functor, A, R](coalgebra: Coalgebra[F, A])(implicit embed: Embed[F, R]): A => R`
	 */
	def coalgebraFromList[A]: Coalgebra[ListF[A, ?], List[A]] = // List[A] => ListF[A, List[A]]
		Coalgebra {
			case Nil => NilF()
			case x :: xs => ConsF(x, xs)
		}

	def fromList[A, B](as: List[A])(implicit ev: Embed[ListF[A, ?], B]): B = {
		scheme.ana[ListF[A, ?], List[A], B](coalgebraFromList[A]).apply(as)
		// Coalgebra[F, A]: A => F[A]
		// Embed[F, R]
		// ana[F, A, R].apply: A => R
		// ---> F[_]: ListF[A, ?]
		// ---> A := List[A]
		// ---> R := B
	}

	// NOTE: compare with scheme.cata[ListF[A, ?], B, List[A]]



	// ----------------------------------------------------------------------------------------------

	def coalgebraFactorial: Coalgebra[ListF[Int, ?], Int] = // Int => ListF[Int, Int]
		Coalgebra {
			case 0 => NilF()
			case n => ConsF(n, n - 1) 	// TODO meaning? how does this create the factorial?
		}

	def factorial[R](n: Int)(implicit ev: Embed[ListF[Int, ?], R]): R = {
		scheme.ana[ListF[Int, ?], Int, R](coalgebraFactorial).apply(n)
		// Coalgebra[F, A]:    A => F[A]
		// Embed[F, R]
		// ana[F, A, R].apply: A => R
		// ---> F[_]: ListF[A, ?]
		// ---> A := Int
		// ---> R := R
	}


	// ----------------------------------------------------------------------------------------------


	def coalgebraRepeat[T](t: T): Coalgebra[ListF[T, ?], Int] = // Int => ListF[T, Int]
		Coalgebra {
			case 0 => NilF()
			case n => ConsF(t: T, (n - 1): Int)

			// NOTE: meaning: repeat givenInt now by n-1 since we used up the first repeat (n)
		}


	def fillByRepeat[T, R](t: T, timesRepeat: Int)(implicit ev: Embed[ListF[T, ?], R]): R =
		scheme.ana[ListF[T, ?], Int, R](coalgebraRepeat[T](t)).apply(timesRepeat)
	// Coalgebra[F, A]:    A => F[A]
	// Embed[F, R]
	// ana[F, A, R].apply: A => R
	// ---> F[_]: ListF[A, ?]
	// ---> A := Int
	// ---> R := R


	// ----------------------------------------------------------------------------------------------



	/**
	 * Hylomorphism operations: hylo = ana . cata
	 *
	 * PROP: `type Algebra[F[_], B] = F[B] => B`
	 *
	 * PROP: `type Coalgebra[F[_], A] = A => F[A]`
	 *
	 * PROP: `def hylo[F[_]: Functor, A, B](algebra: Algebra[F, B], coalgebra: Coalgebra[F, A]): A => B`
	 */
	def same[A]: List[A] => List[A] =
		scheme.hylo[ListF[A, ?], List[A], List[A]](algebraToList[A], coalgebraFromList[A])
	// Algebra[F, B]:    F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := List[A]
	// ---> B := List[A]

	def length[A]: List[A] => Int =
		scheme.hylo[ListF[A, ?], List[A], Int](algebraLength[A], coalgebraFromList[A])
	// Algebra[F, B]:    F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := List[A]
	// ---> B := Int

	def combineAll[A: Monoid]: List[A] => A =
		scheme.hylo[ListF[A, ?], List[A], A](algebraMonoid[A],coalgebraFromList[A])
	// Algebra[F, B]:   F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := List[A]
	// ---> B := A



	// ----------------------------------------------------------------------------------------------

	def algebraReverse[A]: Algebra[ListF[A, ?], List[A]] = // ListF[A, List[A]] => List[A}
		Algebra {
			case NilF() => Nil
			case ConsF(head: A, tail: List[A]) => tail :+ head
			// TODO find out how this actually does reverse (compare to the recursion version: reverse(tail) :+ head
		}

	def factorialHylo: Int => List[Int] =
		scheme.hylo[ListF[Int, ?], Int, List[Int]](algebraReverse[Int], coalgebraFactorial)
	// Algebra[F, B]:    F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := Int
	// ---> B := List[A] := List[Int]



	// ----------------------------------------------------------------------------------------------

	/**
	 * Paramorphism: `F[(R, A)] => A`
	 * - A variation of a catamorphism that gives you access to the input value at every point in the
	 * computation. A paramorphism "eats its argument and keeps it too."
	 * - This means each step has access to both the computed result value as well as the original value.
	 *
	 * NOTE: `type Algebra[F[_], B] = F[B] => B`
	 *
	 * NOTE: `type Coalgebra[F[_], A] = A => F[A]`
	 *
	 * PROP: `type RAlgebra[R, F[_], A]
	 * 			= GAlgebra[F, (R, A), A]
	 * 			= F[(R, A)] => A`
	 *
	 * PROP: `class GAlgebra[F[_], S, A](val run: F[S] => A)`
	 *
	 * PROP: `def para[F[_]: Functor, R, A](algebra: RAlgebra[R, F, A])(implicit project: Project[F, R]): R => A`
	 */

	def ralgebraTail[A]: RAlgebra[List[A], ListF[A, ?], List[A]] = // ListF[A, (List[A], List[A])] => List[A]
		RAlgebra {
			case NilF() => Nil
			case ConsF(head: A, tail@(xs: List[A], ys: List[A])) => xs
		}
	// RAlgebra[R, F[_], A]:   F[(R, A)] => A
	// para[F, R, A].apply: R => A
	// ---> R := List[A]
	// ---> A := List[A]

	def tail[A](rs: List[A])(implicit ev: Project[ListF[A, ?], List[A]]): List[A] = {
		scheme.zoo.para[ListF[A, ?], List[A], List[A]](ralgebraTail[A]).apply(rs)
		// RAlgebra[R, F[_], A]:   F[(R, A)] => A
		// Project[F, R]
		// para[F, R, A].apply: R => A
		// ---> R := List[A]
		// ---> A := List[A]
	}

	// ----------------------------------------------------------------------------------------------

	// ListF[A, (List[A], List[List[A]])] => List[List[A]]
	def ralgebraSliding[A](n: Int): RAlgebra[List[A], ListF[A, ?], List[List[A]]] =
		RAlgebra {
			case NilF() => Nil
			case ConsF(a: A, (rs: List[A], aas: List[List[A]])) => (a :: rs).take(n) :: aas
			// NOTE: meaning: adding the new list (a :: rs) on top of the lsit of lists `aas`
		}

	def sliding[A](n: Int)(rs: List[A])(implicit ev: Project[ListF[A, ?], List[A]]): List[List[A]] = {
		scheme.zoo.para[ListF[A, ?], List[A], List[List[A]]](ralgebraSliding[A](n)).apply(rs)
		// RAlgebra[R, F[_], A]:   F[(R, A) => A
		// Project[F, R]
		// para[F, R, A].apply: R => A
		// ---> F[_]: ListF[A, ?]
		// ---> R := List[A]
		// ---> A := List[List[A]]
	}



	// ----------------------------------------------------------------------------------------------

	/**
	 * Apomorphism: `A => F[Either[R, A]]`
	 * - A variation of an anamorphism that lets you terminate any point of the recursion using a value of the original input type.
	 * - One use case is to return cached/precomputed results during an unfold
	 *
	 * NOTE: `type Algebra[F[_], B] = F[B] => B`
	 *
	 * NOTE: `type Coalgebra[F[_], A] = A => F[A]`
	 *
	 * NOTE: `type RAlgebra[R, F[_], A] = GAlgebra[F, (R, A), A]`
	 *
	 * NOTE: `class GAlgebra[F[_], S, A](val run: F[S] => A)`
	 *
	 * PROP: `type RCoalgebra[R, F[_], A]
	 * 			= GCoalgebra[F, A, Either[R, A]]
	 * 			= A => F[Either[R, A]]`
	 *
	 * PROP: `class GCoalgebra[F[_], A, S](val run: A => F[S])`
	 *
	 * PROP: `def apo[F[_]: Functor, A, R](rcoalgebra: RCoalgebra[R, F, A])(implicit embed: Embed[F, R]): A => R`
	 */

	// List[A] => ListF[A, Either[List[A], List[A]]]
	def rcoalgebraMapHead[A](f: A => A): RCoalgebra[List[A], ListF[A, ?], List[A]] =
		RCoalgebra {
			case Nil => NilF()
			case a :: as => ConsF(f(a), Left(as))
		}

	def mapHead[A](f: A => A)(as: List[A])(implicit ev: Embed[ListF[A, ?], List[A]]): List[A] = {
		scheme.zoo.apo[ListF[A, ?], List[A], List[A]](rcoalgebraMapHead[A](f)).apply(as)
		// RCoalgebra[R, F[_], A]:   A => F[Either[R, A]]
		// Embed[F, R]
		// apo[F, A, R].apply: A => R
		// ---> F[_]: ListF[A, ?]
		// ---> A := List[A]
		// ---> R := List[A]
	}


	// ----------------------------------------------------------------------------------------------

	// RCoalgebra[R, F, A]
	// 		= A => F[Either[R, A]]
	// ----> List[A] => ListF[A, Either[R, List[A]]


	def rcoalgebraInsertElement[A: Order]: RCoalgebra[List[A], ListF[A, ?], List[A]] =
		RCoalgebra {
			case Nil => 					NilF()
			case a :: Nil => 				ConsF(a, Left(Nil))
			case a :: y :: as if a <= y =>	ConsF(a, Left(y :: as))
			case a :: y :: as => 			ConsF(y, Right(a :: as))
		}
	/*def rcoalgebraInsertElement[A: Order]: RCoalgebra[List[A], ListF[A, ?], List[A]] =
		RCoalgebra {
			case Nil => NilF()

			// TODO why not `Right(Nil)`? since Nil is the last and should be "correct" by default?
			case a :: Nil => ConsF(a, Left(Nil))

			// NOTE: Reordered the `a`, so put it at front, then have Leftover y :: as so put it in the "wrong" or `Left` constructor
			case a :: y :: as if a <= y => ConsF(a, Left(y :: as))

			// TODO why is this Right if the above was Left and since now are both ordered???
			case a :: y :: as => 	ConsF(y, Right(a :: as))
		}*/


	def knockback[A: Order](as: List[A])(implicit ev: Embed[ListF[A, ?], List[A]]): List[A] =
		scheme.zoo.apo[ListF[A, ?], List[A], List[A]](rcoalgebraInsertElement[A]).apply(as)



	// RCoalgebra[R, F[_], A]: A => F[Either[R, A]]
	// Embed[F, R]
	// apo[F, A, R].apply: A => R
	// ---> F[_]: ListF[A, ?]
	// ---> A := List[A]
	// ---> R := List[A] or just B


	// TODO: can an algebra be defined with this implicit Embed hanging about? Usually only used for the
	//  morphism-applying functions?

	// ListF[A, List[A]] => List[A]
	def algebraInsertionSort[A: Order](implicit ev: Embed[ListF[A, ?], List[A]]): Algebra[ListF[A, ?],	List[A]] =
		Algebra {
			case NilF() => Nil
			case ConsF(a: A, as: List[A]) => knockback(a :: as)
		}


	// PROP: `def hylo[F[_]: Functor, A, B](algebra: Algebra[F, B],coalgebra: Coalgebra[F, A]): A => B`


	// NOTE: need the extra embed implicit here to satisfy the corresponding extra Embed implicit
	//  from  algebraInsertionSort

	def insertionSort[A: Order](as: List[A])(implicit ev: Embed[ListF[A, ?], List[A]],
									 ev2: Basis[ListF[A, ?], List[A]]): List[A]
	= {
		scheme.hylo[ListF[A, ?], List[A], List[A]](
			algebraInsertionSort[A],
			coalgebraFromList[A]
		).apply(as)

		// Algebra[F, B]: F[B] => B
		// Coalgebra[F, A]: A => F[A]
		// hylo[F, A, B].apply: A => B
		// ---> F[_]: ListF[A, ?]
		// ---> A := List[A]
		// ---> B := List[A] or FixList[A] ... TODO
	}



	// ----------------------------------------------------------------------------------------------

	/**
	 * Histomorphism: `F[Attr[F, A]] => A`
	 * - A variation of an catamorphism (tears down a structure) with previous answers it has given
	 * TODO
	 *
	 * PROP: `type CVAlgebra[F[_], A]
	 * 			= GAlgebra[F, Attr[F, A], A]
	 * 			= F[Attr[F, A]] => A`
	 *
	 * PROP: `class GAlgebra[F[_], S, A](val run: F[S] => A)`
	 *
	 * PROP: `type Attr[F[_], A] = (A,  F[Attr[F, A]])`
	 * `Attr` is a fix point function for types that adds an additional attribute to each node in
	 * the resulting data structure. This is a cofree comonad. Implemented as an obscured alias
	 *
	 * PROP: `def histo[F[_]: Functor, R, A](cvalgebra: CVAlgebra[F, A])(implicit project: Project[F, R]): R => A`
	 */

	// ListF[A, Attr[F, List[A]]] => List[A]
	def cvalgebraOdds[A]: CVAlgebra[ListF[A, ?], List[A]] =
		CVAlgebra {
			case NilF() => Nil
			case ConsF(a: A, _ :< NilF() ) => List(a)
			case ConsF(a1: A, _ :< ConsF(a2, t :< _)) => a1 :: t

			// TODO meaning here? of :< ?? what comes after the `t`?
			// TODO meaning of `t`?
		}

	def odds[A, B](r: B)(implicit ev: Basis[ListF[A, ?], B]): List[A] = {
		scheme.zoo.histo[ListF[A, ?], B, List[A]](cvalgebraOdds[A]).apply(r)
		// CVAlgebra[F, A] = GAlgebra[F, Attr[F, A], A]
		// Attr[F, A] = (A, F[Attr[F, A]])
		// ----> F[Attr[F, A]] => A
		// ----> F[ (A, F[Attr[F, A]]) ] => A
		// ----> F[ (A, F[(A, F[(A, F[(A, F[(A, F[(A, F[Attr[F, A]])])])])])]) ] => A
		// Basis[F, R]
		// histo[F, R, A].apply: R => A
		// ---> F[_]: ListF[A, ?]
		// ---> R := B
		// ---> A := List[A]
	}


	// ----------------------------------------------------------------------------------------------

	// ListF[A, Attr[F, List[A]]] => List[A]
	def cvalgebraEvens[A]: CVAlgebra[ListF[A, ?], List[A]] =
		CVAlgebra {
			case NilF() => Nil
			case ConsF(a: A,  _ :< NilF() ) => Nil
			case ConsF(a1: A,  _ :< ConsF(a2, t :< _)) => a2 :: t
		}

	def evens[A, B](r: B)(implicit ev: Basis[ListF[A, ?], B]): List[A] =
		scheme.zoo.histo[ListF[A, ?], B, List[A]](cvalgebraEvens[A]).apply(r)

	// CVAlgebra[F, A] = GAlgebra[F, Attr[F, A], A]
	// Attr[F, A] = (A, F[Attr[F, A]])
	// ----> F[Attr[F, A]] => A
	// ----> F[ (A, F[Attr[F, A]]) ] => A
	// ----> F[ (A, F[(A, F[(A, F[(A, F[(A, F[(A, F[Attr[F, A]])])])])])]) ] => A
	// Basis[F, R]
	// histo[F, R, A].apply: R => A
	// ---> F[_]: ListF[A, ?]
	// ---> R := B
	// ---> A := List[A]


}


object Example extends App {


	import ListF._
	import FixList._

	// TODO what does this do? does it convert from ListF[A, ?] to List[A]   ????
	implicit def basis[A]: Basis[ListF[A, ?], List[A]] =
		Basis.Default(algebraToList[A], coalgebraFromList[A])


	val listF_12: FixList[Int] = cons(1, wrap(2))
	val listF_4321: FixList[Int] = cons(4, cons(3, cons(2, wrap(1))))
	val to: FixList[Int] = listF_4321
	val from: List[Int] = List(3,1,2,2,4,3,5,1,6,2,1)



	// ------------------------------------------------------------------------------------------

	val resultOfToList: List[Int] = toList[Int, List[Int]](from)

	val resultOfToListFix: List[Int] = toList[Int, FixList[Int]](to)
	println("resultOfToListFix: " + resultOfToListFix)

	/*def toList[A, B](r: B)(implicit ev: Basis[ListF[A, ?], B]): List[A] = {
		scheme.cata[ListF[A, ?], B, List[A]](algebraToList[A]).apply(r)
		// Algebra[F, B]: F[B] => B
		// Project[F, R],   Basis[F, R]
		// cata[F, R, B].apply: R => B
		// ---> F[_]: ListF[A, ?]
		// ---> R := B
		// ---> B := List[A]

		GOAL: Figure out:
		* toList[A, B]
		* return type List[A]

		// GIVEN:
		// argument      | r := to
		// argument type | r : B, to: B, to: Fix[ListF[A, ?]] = FixList[A] := FixList[Int]
		// THEN:
		// ---> A := Int
		// ---> B := FixList[Int]
		// ---> toList[A, B](r) ==> toList[Int, FixList[Int]](to)
		// RETURN TYPE:
		// List[A]
		// ---> List[Int]
	}*/

	// ------------------------------------------------------------------------------------------

	val resultOfCombineAlgebras: (Int, String) = combineAndShowAlgebras[Int, List[Int]](from)
	println("resultOfCombineAlgebras: " + resultOfCombineAlgebras)
	// combine[A, B]
	// ---> B := List[Int]

	val resultOfCombineAlgebrasFix: (Int, String) = combineAndShowAlgebras[Int, FixList[Int]](to)
	Console.println("resultOfCombineAlgebrasFix: " + resultOfCombineAlgebrasFix)


	/*def combineAndShowAlgebras[A: Show : Monoid, B](r: B)(implicit B: Basis[ListF[A, ?], B]): (A, String) = {
		scheme.cata[ListF[A, ?], B, (A, String)](algebraMonoid[A].zip(algebraShow[A])).apply(r)
		// Algebra[F, B]: F[B] => B
		// Project[F, R],   Basis[F, R]
		// cata[F, R, B].apply: R => B
		// ---> F[_]: ListF[A, ?]
		// ---> R := B
		// ---> B := (A, String)


		GOAL: Figure out
		* combineAndShowAlgebras[A, B]
		* (A, String)

		// GIVEN:
		// argument      |   r := to
		// argument type |   r: B, to: Fix[ListF[A, ?]] = FixList[A] = FixList[Int]
		// THEN:
		// ---> A := Int
		// ---> B := FixList[Int]
		// ---> combineAndShowAlgebras[Int, FixList[Int]](to)
		// RETURN TYPE:
		// (A, String)
		// ---> (Int, String)
	}*/


	// ------------------------------------------------------------------------------------------

	val resultOfFactorial: List[Int] = factorial(10) //factorial[FixList[Int]](10)
	Console.println("resultOfFactorial: " + resultOfFactorial)

	/*def factorial[R](n: Int)(implicit ev: Embed[ListF[Int, ?], R]): R = {
		scheme.ana[ListF[Int, ?], Int, R](coalgebraFactorial).apply(n)

		// Coalgebra[F, A]: A => F[A]
		// Embed[F, R]
		// ana[F, A, R].apply: A => R
		// ---> F[_]: ListF[A, ?]
		// ---> A := Int
		// ---> R := R
		// ---> R := R
		TODO HELP HERE how to figure out what is type of R?
		TODO why is the return type (R) is FixList[Int] when not including the basis implicit and why is it just  List[Int] when including the basis implicit?

		GOAL: Figure out
		* factorial[R]
		* return type R

		// GIVEN:
		// Coalgebra[F[_], A]
		// A := Int
		// F := ListF[A, ?]
		// ana[F, A, R].apply: A => R
		// ---> R := HELP ????
	}*/

	val resultFactorialFix: FixList[Int] = factorial[FixList[Int]](7)
	Console.println("resultFactorialFix: " + resultFactorialFix)
	// factorial[R]
	// factorial[FixList[Int]]
	// return type: R
	// ---> R := FixList[Int]

	// ------------------------------------------------------------------------------------------

	val resultOfFill: List[String] = fillByRepeat[String, List[String]]("panda", 10)
	Console.println("resultOfFill: " + resultOfFill)

	/*def fillByRepeat[T, R](t: T, timesRepeat: Int)(implicit ev: Embed[ListF[T, ?], R]): R =
		scheme.ana[ListF[T, ?], Int, R](coalgebraRepeat[T](t)).apply(timesRepeat)

	// Coalgebra[F, A]:    A => F[A]
	// Embed[F, R]
	// ana[F, A, R].apply: A => R
	// ---> F[_]: ListF[A, ?]
	// ---> A := Int
	// ---> R := R


	GOAL: Figure out:
	* fillByRepeat[R]
	* return type R

	GIVEN:
	TODO HELP same problem as for factorial: how to figure out runtime type of R?
	*/

	val resultOfFillFix: FixList[Int] = fillByRepeat[Int, FixList[Int]](3, 10)
	Console.println("resultOfFillFix: " + resultOfFillFix)
	// fillByRepeat[R]
	// fillByRepeat[FixList[Int]]
	// return type: R
	// ---> R := FixList[Int]


	// ------------------------------------------------------------------------------------------

	val resultOfFromList: List[Int] = fromList(from)
	Console.println("resultOfFromList: " + resultOfFromList)

	/*def fromList[A, B](as: List[A])(implicit ev: Embed[ListF[A, ?], B]): B = {
		scheme.ana[ListF[A, ?], List[A], B](coalgebraFromList[A]).apply(as)
		// Coalgebra[F, A]: A => F[A]
		// Embed[F, R]
		// ana[F, A, R].apply: A => R
		// ---> F[_]: ListF[A, ?]
		// ---> A := List[A]
		// ---> R := B

		GOAL: Figure out
		* fromList[A, B]
		* return type B

		GIVEN:
		// argument      | as := from
		// argument type | as:  List[A],  from: List[Int]
		// ----> A := Int
		// ----> B HELP FIGURE OUT why is this equal to FixList[Int]? Coalgebra doesn't yield this as R?

	}*/

	val resultOfFromListFix: FixList[Int] = fromList[Int, FixList[Int]](from)
	Console.println("resultOfFromListFix: " + resultOfFromListFix)
	// fromList[A, B]
	// fromList[Int, FixList[Int]]
	// Embed[ListF[A, ?], B)
	// return type: B
	// ----> B := FixList[Int]

	// ------------------------------------------------------------------------------------------

	val resultOfSame: List[Int] = same[Int](List(1,2,3))
	Console.println("resultOfSame: " + resultOfSame)

	/*def same[A]: List[A] => List[A] =
		scheme.hylo[ListF[A, ?], List[A], List[A]](algebraToList[A], coalgebraFromList[A])
	// Algebra[F, B]:    F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := List[A]
	// ---> B := List[A]

	GOAL: Figure out
	* same[A]
	* arg type: List[A]
	* return type: List[A]
	GIVEN:
	* argument: List[Int]
	----> List[A] := List[Int]
	----> A := Int

	*/

	// ------------------------------------------------------------------------------------------

	val resultOfLength: Int = length[Int](List(1,2,3,4,5))
	Console.println("resultOfLength: " + resultOfLength)

	/*def length[A]: List[A] => Int =
		scheme.hylo[ListF[A, ?], List[A], Int](algebraLength[A], coalgebraFromList[A])
	// Algebra[F, B]:    F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := List[A]
	// ---> B := Int

	GOAL: Figure out
	* length[A]
	* arg type: List[A]
	GIVEN:
	* argument: List[Int]
	----> A := Int
	----> length[Int]

	*/


	// ------------------------------------------------------------------------------------------

	val resultOfCombineAll: Int = combineAll[Int].apply((1 to 1000).toList)
	Console.println("resultOfCombineAll: " + resultOfCombineAll)

	/*
	def combineAll[A: Monoid]: List[A] => A =
		scheme.hylo[ListF[A, ?], List[A], A](algebraMonoid[A],coalgebraFromList[A])
	// Algebra[F, B]:   F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := List[A]
	// ---> B := A

	GOAL: Figure out
	* A
	* arg: List[A]
	GIVEN
	* arg: List[A] is (1 to 1000).toList
	----> A := Int
	----> combineAll[Int].apply(...)
	*/

	val resultOfCombineAllString: String = combineAll[String].apply(List("cats", " ", "meow"))
	Console.println("resultOfCombineAllString: " + resultOfCombineAllString)


	// ------------------------------------------------------------------------------------------

	val resultOfReverseFactorial: List[Int] = factorialHylo(10)
	Console.println("resultOfReverseFactorial: " + resultOfReverseFactorial)

	/*def factorialHylo: Int => List[Int] =
		scheme.hylo[ListF[Int, ?], Int, List[Int]](algebraReverse[Int], coalgebraFactorial)
	// Algebra[F, B]:    F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// hylo[F, A, B].apply: A => B
	// ---> A := Int
	// ---> B := List[A] := List[Int]

     */

	// ------------------------------------------------------------------------------------------

	val resultOfTail: List[Int] = tail[Int](List(1,2,3,4,5,6,7,8))
	Console.println("resultOfTail: " + resultOfTail)

	/*def tail[A](list: List[A])(implicit ev: Project[ListF[A, ?], List[A]]): List[A] = {
		scheme.zoo.para[ListF[A, ?], List[A], List[A]](ralgebraTail).apply(list)
		// RAlgebra[R, F[_], A]:   F[(R, A)] => A
		// Project[F, R]
		// para[F, R, A].apply: R => A
		// ---> R := List[A]
		// ---> A := List[A]

		GOAL: Figure out
		* A
		* arg type: List[A]
		GIVEN:
		* arg type: List[Int]
		----> A := Int
		----> tail[Int]
	}*/

	val resultOfTailString: List[String] = tail[String](List("a", "b", "c", "d", "e"))
	Console.println("resultOfTailString: " + resultOfTailString)

	// ------------------------------------------------------------------------------------------

	val resultOfSlide: List[List[Int]] = sliding[Int](3)((1 to 5).toList)
	Console.println("resultOfSlide: " + resultOfSlide)

	/*def sliding[A](n: Int)(as: List[A])(implicit ev: Project[ListF[A, ?], List[A]]): List[List[A]] = {
		scheme.zoo.para(ralgebraSliding[A](n)).apply(as)
		// RAlgebra[R, F[_], A]:   F[(R, A) => A
		// Project[F, R]
		// para[F, R, A].apply: R => A
		// ---> F[_]: ListF[A, ?]
		// ---> R := List[A]
		// ---> A := List[List[A]]
	}

	GOAL: Figure out:
	* sliding[A]
	* return type: List[List[A]]
	GIVEN:
	* arg type: List[Int]
	* ---> A := Int
	* ---> sliding[Int]
	* ---> return type: List[List[Int]]

	*/
	// ------------------------------------------------------------------------------------------

	val resultOfMapHead: List[Int] = mapHead[Int](x => x + 1)(List(1,2,3,4))
	Console.println("resultOfMapHead: " + resultOfMapHead)

	// ------------------------------------------------------------------------------------------

	//val resultOfSorted: List[String] = insertionSort[String, List[String]](List("b", "f", "g", "a", "x"))
	val resultOfSorted: List[String] = insertionSort[String](List("b", "f", "g", "a", "x"))
	Console.println("resultOfSorted: " + resultOfSorted)

	//val resultOfSortedFix: FixList[Int] = insertionSort[Int, FixList[Int]](from)
	// NOTE: not possible here to make it Fix because the algebra must be written with either Nil or
	//  Fix(Nil) and that will give it away (cannot cement a value while having abstract type)
	//  val resultOfSortedFix =  insertionSort[Int](from)
	//Console.println("resultOfSortedFix: " + resultOfSortedFix)

	/*def insertionSort[A: Order, B](as: List[A])(implicit ev: Embed[ListF[A, ?], List[A]]): List[A] = {
		scheme.hylo[ListF[A, ?], List[A], List[A]](algebraInsertionSort[A], coalgebraFromList[A]).apply(as)

		// Algebra[F, B]: F[B] => B
		// Coalgebra[F, A]: A => F[A]
		// hylo[F, A, B].apply: A => B
		// ---> F[_]: ListF[A, ?]
		// ---> A := List[A]
		// ---> B := List[A]


		GOAL: Figure out
		* insertionSort[A, B]
		* arg type: List[A]
		* return type: List[A]
		GIVEN:
		* arg = from, arg type = List[Int]
		----> A := Int
		----> insertionSort[Int, B]
		TODO help figure out B ???
	}*/

	// ------------------------------------------------------------------------------------------


	val resultOfOdds: List[Int] = odds[Int, List[Int]]((1 to 10).toList)
	Console.println("resultOfOdds: " + resultOfOdds)

	val listF_10: FixList[Int] = cons(1, cons(2, cons(3, cons(4, cons(5, cons(6, cons(7, cons(8, cons(9, wrap(10))))
	))))))

	val resultOfOddsFix: List[Int] = odds[Int, FixList[Int]](listF_10)
	Console.println("resultOfOddsFix: " + resultOfOddsFix)

	/*def odds[A, B](r: B)(implicit ev: Basis[ListF[A, ?], B]): List[A] = {
		scheme.zoo.histo[ListF[A, ?], B, List[A]](cvalgebraOdds[A]).apply(r)
		// CVAlgebra[F, A] = GAlgebra[F, Attr[F, A], A]
		// Attr[F, A] = (A, F[Attr[F, A]])
		// ----> F[Attr[F, A]] => A
		// ----> F[ (A, F[Attr[F, A]]) ] => A
		// ----> F[ (A, F[(A, F[(A, F[(A, F[(A, F[(A, F[Attr[F, A]])])])])])]) ] => A
		// Basis[F, R]
		// histo[F, R, A].apply: R => A
		// ---> F[_]: ListF[A, ?]
		// ---> R := B
		// ---> A := List[A]

		GOAL: Figure out
		* odds[A, B]
		* return type: List[A]
		GIVEN:
		* arg type List[Int], r: B
		---> R := B := List[Int]
		GIVEN:
		* histo[F,R,A], histo[ListF, B, List[A]]
		* CVAlgebra[ListF[Int(?), ?]]
		---> A := Int TODO figure out a better way to calculate what is the type (A)

	}*/

	// ------------------------------------------------------------------------------------------

	val resultEvensFix: List[Int] = evens[Int, FixList[Int]](listF_10)
	Console.println("resultEvensFix: " + resultEvensFix)

	/*def evens[A, B](r: B)(implicit ev: Basis[ListF[A, ?], B]): List[A] =
		scheme.zoo.histo[ListF[A, ?], B, List[A]](cvalgebraEvens[A]).apply(r)

	// CVAlgebra[F, A] = GAlgebra[F, Attr[F, A], A]
	// Attr[F, A] = (A, F[Attr[F, A]])
	// ----> F[Attr[F, A]] => A
	// ----> F[ (A, F[Attr[F, A]]) ] => A
	// ----> F[ (A, F[(A, F[(A, F[(A, F[(A, F[(A, F[Attr[F, A]])])])])])]) ] => A
	// Basis[F, R]
	// histo[F, R, A].apply: R => A
	// ---> F[_]: ListF[A, ?]
	// ---> R := B
	// ---> A := List[A]

	GOAL: Figure out:
	* evens[A, B]
	GIVEN:
	* arg type: List[Int]
	----> B := List[Int]
	TODO figure out how to figure out type of A
	*/


}



