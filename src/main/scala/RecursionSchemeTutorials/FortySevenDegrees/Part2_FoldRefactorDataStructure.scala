package RecursionSchemeTutorials.FortySevenDegrees

/**
 *
 */

import cats._ //functor
import cats.implicits._ //for fmap to work
import utils.FoldUtils._




object Part2_FoldRefactorDataStructure extends App {

	import Operations._


	/**
	 * REFACTORING 2: Factor out the Data Structure
	 *
	 * So far the implementation is tightly coupled to a list, the data structure.
	 *
	 * Let us break down the function into steps to better understand where the `List` type appears. The point is to
	 * illustrate which steps depend on `List` and eliminate them.
	 *
	 * The function `foldRight` is unpacking the data structure. It is proceeding with a recursive call in the case
	 * when the list is not empty. Finally, it is computing the result using `f`.
	 *
	 *
	 * To summarize, the steps are:
	 *
	 * 1. Unpacking / projecting the data structure (creating a list with :: operator)
	 *
	 * 2. Recursion: call self for the nested structure
	 *
	 * 3. Computation using `f`
	 */

	def foldRight_old[E, B](f: Option[(E, B)] => B): List[E] => B =

		new (List[E] => B) { kernel =>
			def apply(init: List[E]): B = init match {
				case Nil => ??? // f(None)
				case head :: tail => ???	// f(Some((head, kernel(tail))))
			}
		}

	/**
	 * How to reimplement `foldRight`?
	 *
	 * Answer: Instead of having one function `apply`, we introduce three new functions, one per step.
	 *
	 */

	/*def foldRight[E, B](f: Option[(E, B)] => B): List[E] => B = {
		new (List[E] => B ) { kernel =>
			def step1 = ??? // unpack
			def step2 = ??? // recurse
			def step3 = ??? // compute

			def apply(init: List[E]): B = step3(step2(step1(init)))
		}
	}*/

	/**
	 * `step1` (unpack) must take `List[E]` as parameter. Given that we can end up only with `Nil` or a `head` or
	 * `tail`, the most general possible return type could be `Option[E, List[E]]`. It forces us to accept it as a
	 * parameter for `step2` (recurse). Thus, `step1 :: List[E] => Option[E, List[E]]`
	 *
	 *
	 * `step2` (recurse) must now take the result type of `step1` as its parameter type (`Option[E, List[E]]`).
	 * It should be noted that `step3` (compute) is essentially `f` computing a result, and so `step3` parameter
	 * type will be the return type of `step2` (`Option[(E, B)])`. Thus: `step2 :: Option[E, List[E]] => Option[(E,
	 * B)]`
	 *
	 * `step3` (compute) has parameter type equal to return type of `step2` and its return type will be the end
	 * return type of `foldRight`, the result type of `f`, which is `B`. Thus, `step3 :: Option[(E, B)] => B`
	 */

	def foldRight_stepTypes[E, B](f: Option[(E, B)] => B): List[E] => B = {
		new (List[E] => B) {

			kernel =>

			def step1_unpack: List[E] => Option[(E, List[E])] = ???
			def step2_recurse: Option[(E, List[E])] => Option[(E, B)] = ???
			def step3_compute: Option[(E, B)] => B = ???

			def apply(init: List[E]): B = step3_compute(step2_recurse(step1_unpack( init )))
		}
	}

	/**
	 * Now writing the step-functions:
	 *
	 * `step1_unpack` must simply take a list and wrap it in an option
	 *
	 * `step2_recurse` uses the `kernel :: List[E] => B` to convert the given list in the option to a type of `B`.
	 *
	 * `step3_compute` is just `f`
	 */
	def foldRight_steps[E, B](f: Option[(E, B)] => B): List[E] => B = {
		new (List[E] => B) {

			kernel =>

			def step1_unpack: List[E] => Option[(E, List[E])] = {
				_ match { // matching on the given list parameter
					case Nil => None
					case head :: tail => Some( (head, tail) )
				}
			}

			def step2_recurse: Option[(E, List[E])] => Option[(E, B)] = {
				_ match {
					case None => None
					case Some((head, tail)) => Some((head, kernel(tail))) // applying kernel to squash the tail to type `B`
				}
			}

			def step3_compute: Option[(E, B)] => B = f // same as f computation


			def apply(init: List[E]): B = step3_compute(step2_recurse(step1_unpack( init )))
		}
	}

	// TESTING
	assert(foldRight_steps(prodOpt)(list) == 5040, "Test: foldRight_steps")
	assert(list.foldRight(1)(prod) == 5040, "Test: foldRight original on list (steps)")


	/**
	 * Introducing some type abstractions:
	 */
	def foldRight_stepsAbstract[E, B](f: Option[(E, B)] => B): List[E] => B = {

		type O[X] = Option[(E, X)] // X = the variable type
		type L = List[E]

		new (L => B) {

			kernel =>

			def step1_unpack: L => O[L] = {
				_ match { // matching on the given list parameter
					case Nil => None
					case head :: tail => Some( (head, tail) )
				}
			}

			def step2_recurse: O[L] => O[B] = {
				_ match {
					case None => None
					case Some((head, tail)) => Some((head, kernel(tail))) // applying kernel to squash the tail
					// to type `B`
				}
			}

			def step3_compute: O[B] => B = f // same as f computation


			def apply(init: L): B = step3_compute(step2_recurse(step1_unpack( init )))
		}
	}
	// TESTING
	assert(foldRight_stepsAbstract(prodOpt)(list) == 5040, "Test: foldRight_stepsAbstract")


	/**
	 * The `step2` resembles a `Functor` because it maps over a value in the `Option` as follows: `O[L] => O[B]`
	 */
	def foldRight_functor[E, B](f: Option[(E, B)] => B): List[E] => B = {

		type O[X] = Option[(E, X)] // X = the variable type
		type L = List[E]

		implicit val O = Functor[Option].compose[(E, ?)]

		new (L => B) {
			kernel =>

			//projecting / unpacking the data structure to Option[data structure]
			def step1_unpack: L => O[L] = {
				_ match { // matching on the given list parameter
					case Nil => None
					case head :: tail => Some( (head, tail) )
				}
			}

			def step2_recurse: O[L] => O[B] = _.fmap(kernel)

			def step3_compute: O[B] => B = f // same as f computation

			def apply(init: L): B = step3_compute(step2_recurse(step1_unpack( init )))
		}
	}

	// TESTING
	assert(foldRight_functor(prodOpt)(list) == 5040, "Test: foldRight_functor")

	/**
	 * Now we can claim that the only remaining dependency on the datastructure `List[E]` is the `step1` function.
	 * Thus if we refactor it to be outside of `foldRight` then we can make the definition of the fold as abstract
	 * as possible.
	 *
	 * (1) The first thing to refactor is the type `O[X] = Option[(E, X)]`. We can use the following
	 * "functor-pattern":
	 *
	 * `type ListF[A, B] = Option[(A, B)]`
	 *
	 * (2) The second step is to get rid of `List[E]` and `E` completely from the signature of the fold and leave
	 * `L` as a type pameter of `foldRight` to represent our data structure. The functor restriction also migrates
	 * to the type parameters.
	 *
	 * Now we can factor out the `step1` and call it `project` as it is projecting our data
	 * structure:
	 *
	 * old: `def foldRight_functor[E, B](f: Option[(E, B)] => B): List[E] => B`
	 *
	 * new: `def foldRight_project[F[_]: Functor, L, B](f: F[B] => B)(project: L => F[L]): L => B`
	 */
	type ListF[A, B] = Option[(A, B)]

	//TODO study how this defines `ListF` to be a functor - how does compose work here?
	// How does this relate to my old way of declaring functor instance by defining `map`?
	implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

	//NOTE step 1 = unpack
	def projectList[E]: List[E] => ListF[E, List[E]] = _ match { // matching on List[E] param
		case Nil => None
		case head :: tail => Some( (head, tail) )
	}

	//This definition just collapses all the step3, 2, 1 together.
	def foldRight_project[F[_]: Functor, L, B](f: F[B] => B)(project: L => F[L]): L => B = {
		new (L => B) {
			kernel =>

			def apply(init: L): B = f( project(init).fmap(kernel)  )

			// step 1a unpack) project :: L => F[L]
			// step 1b unpack) project(init) :: F[L]
			// step 2 recurse) F[L].fmap(kernel: L => B) ==> F[B}
			// step 3 compute) f( F[B] ) ==> B
		}
	}

	assert( foldRight_project(prodFList)(projectList).apply(list) == 5040, "Test: foldRight_project")
}
