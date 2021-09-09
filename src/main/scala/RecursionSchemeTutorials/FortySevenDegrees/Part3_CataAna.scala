package RecursionSchemeTutorials.FortySevenDegrees

/**
 *
 */



import cats._
import cats.implicits._

import scala.language.higherKinds

import Operations.{list, prodFList3, rangeFList3/*, prodCataAlgebra, rangeAnaCoalgebra*/}


object Part3_CataAna extends App {


	/**
	 * REFACTORING 3: Catamorphism and Anamorphism
	 *
	 * The function `foldRight` is a `catamorphism` and `unfold` is an `anamoprhism`
	 *
	 *
	 */

	type ListF[A, B] = Option[(A, B)]

	//TODO study how this defines `ListF` to be a functor - how does compose work here?
	// How does this relate to my old way of declaring functor instance by defining `map`?
	implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]



	//NOTE step 1 = unpack (for foldRight)
	def projectList[E]: List[E] => ListF[E, List[E]] = _ match { // matching on List[E] param
		case Nil => None
		case head :: tail => Some( (head, tail) )
	}



	//This definition just collapses all the step3, 2, 1 together.
	def foldRight_project[F[_]: Functor, L, B](f: F[B] => B)(project: L => F[L]): L => B = {
		new (L => B) {
			kernel =>

			// step3_compute(step2_recurse(project(init)))
			// (O[B] => B) . (O[L] => O[B]) . (L => O[L]) . (B)
			// (F[B] => B) . (F[L] => F[B]) . (L => F[L]) . (B)
			def apply(init: L): B = f( project(init).fmap(es => kernel(es))  )

			// step 1: unpack) project :: L => F[L] ..... project(init)
			// step 2: recurse) F[L] => F[B] ............ project(init).fmap(kernel)
			// step 3: compute) f :: F[B] => B .......... f(project(init).fmap(kernel))
		}
	}




	//NOTE step 3 = unpack (for unfold)
	def embedList[E]: ListF[E, List[E]] => List[E] = /*_ match*/ {
		case None => Nil
		case Some((e, es)) => e :: es
	}

	def unfold_embed[F[_]: Functor, L, B](f: B => F[B])(embed: F[L] => L): B => L = {

		new (B => L) {
			kernel =>

			// step3_unpack(step2_recurse(step1_compute(init)))
			// (O[L] => L) . (O[B] => O[L]) . (B => O[B]) . (B)
			// (F[L] => L) . (F[B] => F[L]) . (B => F[B]) . (B)
			def apply(init: B): L = embed( f(init).fmap(b => kernel(b)) )

			// step 1: compute) f :: B => F[B} ...... f(init)
			// step 2: recurse) F[B] => F[L] ........ f(init).fmap(kernel)
			// step 3: embed) F[L] => L ............. embed(f(init).fmap(kernel))
		}
	}


	/**
	 * The functions `foldRight` and `unfold` as they are implemented are actually equivalent
	 * to `cata` and `ana` respectively
	 */
	def cata_raw[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B = {
		new (S => B) { kernel =>
			def apply(init: S): B = f( project(init).fmap(b => kernel(b)) )
		}
	}

	def ana_raw[F[_]: Functor, S, A](f: A => F[A])(embed: F[S] => S): A => S = {
		new (A => S) {kernel =>
			def apply(init: A): S = embed( f(init).fmap(a => kernel(a)) )
		}
	}



	// TESTING
	assert( cata_raw(prodFList3)(projectList).apply(list) == 5040, "Test: cata_raw")

	assert( ana_raw(rangeFList3)(embedList).apply(5) == List(5,4,3,2,1), "Test: ana_raw" )



	/**
	 * `F[A] => A` is often called an `Algebra` in category theory and its dual `A => F[A]` is called `Coalgebra`
	 */
	type Algebra[F[_], A] = F[A] => A
	type Coalgebra[F[_], A] = A => F[A]

	//NOTE step 3 = unpack (for unfold)
	def embedAlgebra[E]: Algebra[ListF[E, ?], List[E]]  = {
		case None => Nil
		case Some((e, es)) => e :: es
	}

	def projectCoalgebra[E]: Coalgebra[ListF[E, ?], List[E]] = /*_ match*/ { // matching on
		// List[E] param
		case Nil => None
		case e :: es => Some( (e, es) )
	}

	def cata[F[_]: Functor, S, B](algebra: Algebra[F, B])(project: Coalgebra[F, S]): S => B = {
		new (S => B) { kernel =>

			def apply(input: S): B =
				algebra(project(input).fmap(b => kernel(b)))
		}
	}

	def ana[F[_]: Functor, S, A](coalgebra: Coalgebra[F, A])(embed: Algebra[F, S]): A => S = {
		new (A => S) {kernel =>

			def apply(input: A): S =
				embed( coalgebra(input).fmap(a => kernel(a)) )
		}
	}

	val prodCataAlgebra: Algebra[ListF[Int, ?], Int] = {
		case None => 1
		case Some((x, y)) => x * y
	}

	val rangeAnaCoalgebra: Coalgebra[ListF[Int, ?], Int] = {
		v => {
			if (v <= 0) None
			else Some((v, v - 1))
		}
	}


	// TESTING
	//TODO why do I get this issue here? https://hyp.is/yHL2ZhDSEeyuLVdZRmGpYw/www.47deg.com/blog/recursion-schemes-introduction/

	// HELP fix error but how?
	// HELP: type mismatch;
	//  found   : RecursionSchemeTutorials.FortySevenDegrees.Part3_CataAna.Algebra[[β$4$]Option[(Int, β$4$)],Int]
	//    (which expands to)  Option[(Int, Int)] => Int
	//  required: RecursionSchemeTutorials.FortySevenDegrees.Part3_CataAna.Algebra[F,Any]
	//    (which expands to)  F[Any] => Any
	//    assert( cata(prodCataAlgebra)(projectCoalgebra).apply(List(1,2,3,4,5,6,7)) == 5040, "Test: cata")

	//commenting out for now
	/*assert( cata(prodCataAlgebra)(projectCoalgebra).apply(List(1,2,3,4,5,6,7)) == 5040, "Test: cata")

	assert( ana(rangeAnaCoalgebra)(embedAlgebra).apply(5) == List(5,4,3,2,1), "Test: ana" )*/


}
