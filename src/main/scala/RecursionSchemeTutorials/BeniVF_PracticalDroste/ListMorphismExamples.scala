package RecursionSchemeTutorials.BeniVF_PracticalDroste


import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.data.Fix
//import higherkindness.droste.syntax.FixSyntax._
import higherkindness.droste.syntax.all._
import higherkindness.droste.syntax._



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
	type Fixed[A] = Fix[ListF[A, ?]]

	object Fixed {

		def wrap[A](a: A): Fixed[A]                    = {
			// TODO source code uses .fix - cannot get this loaded?
			Fix[ListF[A, ?]](ConsF(a, Fix[ListF[A, ?]](NilF())))
		}
		def cons[A](head: A, tail: Fixed[A]): Fixed[A] = {
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

	def toList[A, B](r: B)(implicit ev: Basis[ListF[A, ?], B]): List[A] = {
		scheme.cata[ListF[A, ?], B, List[A]](algebraToList[A]).apply(r)
		// Algebra[F, B]: F[B] => B
		// Project[F, R],   Basis[F, R]
		// apply: R => B
		// cata[F, R, B]
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


	def combineAndShowAlgebras[A: Show : Monoid, B](r: B)(implicit B: Basis[ListF[A, ?], B]): (A, String) = {
		scheme.cata[ListF[A, ?], B, (A, String)](algebraMonoid[A].zip(algebraShow[A])).apply(r)
		// Algebra[F, B]: F[B] => B
		// Project[F, R],   Basis[F, R]
		// apply: R => B
		// cata[F, R, B]
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
		// apply: A => R
		// ana[F, A, R]
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
		// Coalgebra[F, A]: A => F[A]
		// Embed[F, R]
		// apply: A => R
		// ana[F, A, R]
		// ---> F[_]: ListF[A, ?]
		// ---> A := Int
		// ---> R := R
	}


	// ----------------------------------------------------------------------------------------------


	def coalgebraRepeat(givenInt: Int): Coalgebra[ListF[Int, ?], Int] = // Int => ListF[Int, Int]
		Coalgebra {
			case 0 => NilF()
			case n => ConsF(givenInt, n - 1) // NOTE: meaning: repeat givenInt now by n-1 since we used up the first repeat (n)
		}


	def fillByRepeat[R](numberToFillWith: Int, timesRepeat: Int)(implicit ev: Embed[ListF[Int, ?], R]): R =
		scheme.ana[ListF[Int, ?], Int, R](coalgebraRepeat(numberToFillWith)).apply(timesRepeat)
	// Coalgebra[F, A]: A => F[A]
	// Embed[F, R]
	// apply: A => R
	// ana[F, A, R]
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
	// Algebra[F, B]: F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// apply: A => B
	// hylo[F, A, B]
	// ---> A := List[A]
	// ---> B := List[A]

	def length[A]: List[A] => Int =
		scheme.hylo[ListF[A, ?], List[A], Int](algebraLength[A], coalgebraFromList[A])
	// Algebra[F, B]: F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// apply: A => B
	// hylo[F, A, B]
	// ---> A := List[A]
	// ---> B := Int

	def combineAll[A: Monoid]: List[A] => A =
		scheme.hylo[ListF[A, ?], List[A], A](algebraMonoid[A],coalgebraFromList[A])
	// Algebra[F, B]: F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// apply: A => B
	// hylo[F, A, B]
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
	// Algebra[F, B]: F[B] => B
	// Coalgebra[F, A]: A => F[A]
	// apply: A => B
	// hylo[F, A, B]
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
	 * PROP: `type RAlgebra[R, F[_], A] = GAlgebra[F, (R, A), A]`
	 * `F[(R, A)] => A`
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
	// RAlgebra[R, F[_], A]: F[(R, A)] => A
	// apply: R => A
	// para[F, R, A]
	// ---> R := List[A]
	// ---> A := List[A]

	def tail[A](list: List[A])(implicit ev: Project[ListF[A, ?], List[A]]): List[A] = {
		scheme.zoo.para[ListF[A, ?], List[A], List[A]](ralgebraTail).apply(list)
		// RAlgebra[R, F[_], A],    Project[F, R]
		// apply: R => A
		// para[F, R, A]
		// ---> R := List[A]
		// ---> A := List[A]
	}

	// ----------------------------------------------------------------------------------------------

	// ListF[A, (List[A], List[List[A]])] => List[List[A]]
	def algebraSliding[A](n: Int): RAlgebra[List[A], ListF[A, ?], List[List[A]]] =
		RAlgebra {
			case NilF() => Nil
			case ConsF(a: A, (as: List[A], aas: List[List[A]])) => (a :: as).take(n) :: aas
			// NOTE: meaning: adding the new list (a :: as) on top of the lsit of lists `aas`
		}

	def sliding[A](n: Int)(as: List[A])(implicit ev: Project[ListF[A, ?], List[A]]): List[List[A]] = {
		scheme.zoo.para(algebraSliding[A](n)).apply(as)
		// RAlgebra[R, F[_], A]:   F[(R, A) => A
		// Project[F, R]
		// apply: R => A
		// para[F, R, A] : F[(R, A)] => A
		// ---> F[_]: ListF[A, ?]
		// ---> R := List[A]
		// ---> A := List[List[A]]
	}



	// ----------------------------------------------------------------------------------------------

	/**
	 * Apomorphism: A => F[Either[R, A]]
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
	 * PROP: `type RCoalgebra[R, F[_], A] = GCoalgebra[F, A, Either[R, A]]`
	 * `A => F[Either[R, A]]`
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
		scheme.zoo.apo(rcoalgebraMapHead[A](f)).apply(as)
		// RCoalgebra[R, F[_], A]: A => F[Either[R, A]]
		// Embed[F, R]
		// apply: A => R
		// apo[F, A, R]
	}
}



