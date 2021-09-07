package RecursionSchemeTutorials.FortySevenDegrees

/**
 *
 */


import cats._
import cats.implicits._


import Operations._

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
}
