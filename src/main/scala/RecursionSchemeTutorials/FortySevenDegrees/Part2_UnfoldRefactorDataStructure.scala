package RecursionSchemeTutorials.FortySevenDegrees

/**
 *
 */


import cats._
import cats.implicits._


import scala.language.higherKinds

import Operations.{list, rangeOpt, rangeFList2}



object Part2_UnfoldRefactorDataStructure extends App {

	/**
	 * REFACTORING 2: Facotr out the Data Structure
	 *
	 * `unfold` works opposite order of `foldRight`:
	 *
	 * 1. computation using `f`
	 *
	 * 2. recursion: call self for the nested structure
	 *
	 * 3. unpacking / projecting the data structure with the :: operator.
	 */
	def unfold_anon[E, B](f: B => Option[(E, B)]): B => List[E] = {

		new (B => List[E]) { kernel =>
			def apply(init: B): List[E] = f(init) match {
				case None => Nil
				case Some((e, b)) => e :: kernel(b)
			}
		}
	}

	//Writing the types of the steps (just mirror image of the foldRight step type signatures):

	def unfold_stepTypes[E, B](f: B => Option[(E, B)]): B => List[E] = {

		new (B => List[E]) {
			kernel =>

			def step1_compute: B => Option[(E, B)] = ???
			def step2_recurse: Option[(E, B)] => Option[(E, List[E])] = ???
			def step3_unpack: Option[(E, List[E])] => List[E] = ???

			def apply(init: B): List[E] = step3_unpack(step2_recurse(step1_compute(init)))
		}
	}


	/**
	 * Implementing the steps functionality:
	 *
	 * `step1_compute` == f
	 *
	 * `step2_recurse` must take the result of f(init) and use that as argument to `kernel :: B => List[E]` to
	 * convert the `B` in the option to a `List[E]`
	 *
	 * `step3_unpack` must take the result of the recursion step and extract the list from the option
	 */
	def unfold_steps[E, B](f: B => Option[(E, B)]): B => List[E] = {

		new (B => List[E]) {
			kernel =>

			// same as step3_compute in foldRight (but f type is mirror image)
			def step1_compute: B => Option[(E, B)] = f

			// same as step2_recurse in foldRight (but kernel type is different)
			def step2_recurse: Option[(E, B)] => Option[(E, List[E])] = _ match { //matching on the given option[e,b] arg
				case None => None
				case Some((e, b)) => Some((e, kernel(b)))
			}

			//mirror image of step1_unpack in foldRight
			def step3_unpack: Option[(E, List[E])] => List[E] = _ match { //  matching on the given option arg
				case None => Nil
				case Some((e, es)) => e :: es
			}

			def apply(init: B): List[E] = step3_unpack(step2_recurse(step1_compute(init)))

		}
	}


	//TESTING
	assert(unfold_steps(rangeOpt)(5) == List(5,4,3,2,1),
		"Test: unfold_steps")


	/**
	 * Introducing type aliases:
	 */
	def unfold_stepsAbstract[E, B](f: B => Option[(E, B)]): B => List[E] = {

		type O[X] = Option[(E, X)] // X = the variable type
		type L = List[E]


		new (B => L) {
			kernel =>

			// same as step3_compute in foldRight (but f type is mirror image)
			def step1_compute: B => O[B] = f

			// same as step2_recurse in foldRight (but kernel type is different)
			def step2_recurse: O[B] => O[L] = _ match { //matching on the given option[e,b] arg
				case None => None
				case Some((e, b)) => Some((e, kernel(b)))
			}

			//mirror image of step1_unpack in foldRight
			def step3_unpack: O[L] => L = _ match { //  matching on the given option arg
				case None => Nil
				case Some((e, es)) => e :: es
			}

			def apply(init: B): L = step3_unpack(step2_recurse(step1_compute(init)))

		}
	}


	//TESTING
	assert(unfold_stepsAbstract(rangeOpt)(5) == List(5,4,3,2,1),
		"Test: unfold_stepsAbstract")



	/**
	 * The `step2_recurse` resembles a `Functor` because it maps over a value in the `Option` as follows: `O[B] => O[L]`
	 */
	def unfold_functor[E, B](f: B => Option[(E, B)]): B => List[E] = {

		type O[X] = Option[(E, X)] // X = the variable type
		type L = List[E]


		implicit val O = Functor[Option].compose[(E, ?)]


		new (B => L) {
			kernel =>

			// same as step3_compute in foldRight (but f type is mirror image)
			def step1_compute: B => O[B] = f

			// same as step2_recurse in foldRight (but kernel type is different)
			// OLD: case Some((e, b)) => Some((e, kernel(b)))
			// NEW: converting this step to use Functor's fmap:
			def step2_recurse: O[B] => O[L] = optEB => optEB.fmap(b => kernel(b)) //_.fmap(kernel)

			//mirror image of step1_unpack in foldRight
			def step3_unpack: O[L] => L = _ match { //  matching on the given option arg
				case None => Nil
				case Some((e, es)) => e :: es
			}

			// step3_unpack(step2_recurse(step1_compute(init)))
			// (O[L] => L) . (O[B] => O[L]) . (B => O[B]) . (B)
			def apply(init: B): L = step3_unpack(step2_recurse(step1_compute(init)))

		}
	}


	//TESTING
	assert(unfold_functor(rangeOpt)(5) == List(5,4,3,2,1),
		"Test: unfold_functor")


	/**
	 * Separating Out the List Data Structure Dependency:
	 *
	 * Moving out the `step3_unpack` and calling it `embed` because it is embedding or condensing our data structure:
	 *
	 * old: `def unfold_functor[E, B](f: B => Option[(E, B)]): B => List[E]`
	 *
	 * new: `def unfold_embed`
	 */
	type ListF[A, B] = Option[(A, B)]

	implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]


	//NOTE step 3 = unpack
	def embedList[E]: ListF[E, List[E]] => List[E] = {
		_ match {
			case None => Nil
			case Some((e, es)) => e :: es
		}
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

	// TESTING
	// TODO: why here need to call apply explicitly while above need not?
	assert(unfold_embed(rangeFList2)(embedList).apply(5) == List(5,4,3,2,1), "Test: unfold_embed")

}
