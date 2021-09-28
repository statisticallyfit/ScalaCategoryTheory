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
	 * PROP: `type Algebra[F[_], A] = F[A] => A`
	 *
	 * PROP: `def cata[F[_]: Functor, R, B](algebra: Algebra[F, B])(implicit project: Project[F, R]): R => B`
	 */
	// ----------------------------------------------------------------------------------------------

	// Algebra[F, B] = Algebra[ListF[A, ?], List[A]]
	// Project[F, R]
	// ---> F == ListF[A, ?]
	// ---> B == List[A]
	// ---> R == B
	// apply(R) => B
	// ---> apply(B) => List[A]
	// ---> Basis[F, R] ---> Basis[ListF[A, ?], B]
	// ---> Project[F, R] ---> Project[ListF[A, ?], B]
	def algebraToList[A]: Algebra[ListF[A, ?], List[A]] = // ListF[A, List[A]] => List[A]
		Algebra {
			case NilF() => Nil
			case ConsF(head : A, tail : List[A]) => head :: tail
		}


	// NOTE: need the implicit Basis because otherwise ERROR: no implicits for Project[F, R]

	def toList[A, B](b: B)(implicit B: Basis[ListF[A, ?], B]): List[A] = {
		scheme.cata[ListF[A, ?], B, List[A]](algebraToList[A]).apply(b)
		//scheme.cata[F[_]: Functor, R, B]
		// Basis[F[_], R]
		// Project[F[_], R]
		// apply: R_ => B_
		// ---> R_ := B
		// ---> B_ := List[A]
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


	def combineAndShowAlgebras[A: Show : Monoid, B](b: B)(implicit B: Basis[ListF[A, ?], B]): (A, String) = {
		scheme.cata[ListF[A, ?], B, (A, String)](algebraMonoid[A].zip(algebraShow[A])).apply(b)
		// scheme.cata[F[_]: Functor, R, B]
		// Basis[F[_]: Functor, R]
		// Project[F[_]: Functor, R]
		// apply: R => B
		// ---> F[_] := ListF[A, ?]
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

	// Embed[F[_], R]
	// ---> F[_] := ListF[A, ?]
	// ---> R := B
	// scheme.ana[F, A, R] : A => R
	// apply: A => R
	// ---> A := List[A]
	// ---> R := B
	def fromList[A, B](list: List[A])(implicit B: Embed[ListF[A, ?], B]): B =
		scheme.ana[ListF[A, ?], List[A], B](coalgebraFromList[A]).apply(list)

	// NOTE: compare with scheme.cata[ListF[A, ?], B, List[A]]



	// ----------------------------------------------------------------------------------------------

	def coalgebraFactorial[A]: Coalgebra[ListF[Int, ?], Int] = // Int => ListF[Int, Int]
		Coalgebra {
			case 0 => NilF()
			case n => ConsF(n, n - 1) 	// TODO meaning? how does this create the factorial?
		}

	def factorial[R](n: Int)(implicit ev: Embed[ListF[Int, ?], R]): R = {
		scheme.ana[ListF[Int, ?], Int, R](coalgebraFactorial).apply(n)
		// Embed[F, A, R]
		// apply: A => R
		// ---> F[_] := ListF[Int, ?]
		// ---> R := R
		// ---> A := Int
	}


	// ----------------------------------------------------------------------------------------------


	def coalgebraRepeat(givenInt: Int): Coalgebra[ListF[Int, ?], Int] = // Int => ListF[Int, ?]
		Coalgebra {
			case 0 => NilF()
			case n => ConsF(givenInt, n - 1) // TODO meaning of this?
		}

	// Embed[F, R] = Embed[ListF[Int, ?], R]
	// apply: A => R
	// Coalgebra[F, A]
	// ---> F[_] := ListF[Int, ?]
	// ---> R := R := Int
	// ---> A := Int
	def fillByRepeat[R](numberToFillWith: Int, timesRepeat: Int)(implicit ev: Embed[ListF[Int, ?], R]): R =
		scheme.ana[ListF[Int, ?], Int, R](coalgebraRepeat(numberToFillWith)).apply(timesRepeat)
}



