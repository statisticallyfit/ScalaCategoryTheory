package RecursionSchemeTutorials.BeniVF_PracticalDroste


import higherkindness.droste._
import higherkindness.droste.data.Fix
import higherkindness.droste.data.Mu.drosteBasisForMu
import higherkindness.droste.data.Nu.drosteBasisForNu
//import higherkindness.droste.syntax.FixSyntax._
import higherkindness.droste.syntax.all._
import higherkindness.droste.syntax._

import higherkindness.droste.data._
import list._
import Basis._ // must import drostebasisforfix (implicit)

import cats._
import cats.implicits._
import cats.syntax.all._

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


	// Catamorphism operations: F[A] => A ----------------------------------------------------------------



	// Algebra[F, B] = Algebra[ListF[A, ?], List[A]]
	// ---> F == ListF[A, ?]
	// ---> B == List[A]
	def algebraToList[A]: Algebra[ListF[A, ?], List[A]] = // ListF[A, List[A]] => List[A]
		Algebra {
			case NilF() => Nil
			case ConsF(head : A, tail : List[A]) => head :: tail
		}

	// Project[F, R]
	// ---> R == B
	// apply(R) => B
	// ---> apply(B) => List[A]
	// ---> Basis[F, R] ---> Basis[ListF[A, ?], B]
	// ---> Project[F, R] ---> Project[ListF[A, ?], B]
	def toList[A, B](b: B)(implicit B: Basis[ListF[A, ?], B]): List[A] =
		scheme.cata(algebraToList[A]).apply(b)



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

	
}


