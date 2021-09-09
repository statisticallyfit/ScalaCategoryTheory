package RecursionSchemeTutorials.FortySevenDegrees



/**
 *
 */
import utils.FoldUtils._


import scala.language.higherKinds


object Part1_FoldUnfoldBasic extends App {

	import Operations._

	/**
	 * Fold Right definition:
	 *
	 * `def foldRight[B](z: B)(op: (A, B) => B): B`
	 *
	 *
	 * Unfold Definition:
	 *
	 * `def unfold[A, S](init: S)(f: (S) => Option[(A, S)]): List[A]`
	 *
	 * Alternate FoldRight definition: (with List, to show similarity with unfold)
	 *
	 * `def foldRight[A, B](init: List[A])(z: B)(op: (A, B) => B): B`
	 *
	 *
	 */

	def foldRight_nonDual[A, B](init: List[A])(z: B)(op: (A, B) => B): B = init match {
		case Nil => z
		case head :: tail => op(head, foldRight_nonDual(tail)(z)(op))
	}




	assert(foldRight_nonDual(list)(seed)(prod) == 5040, "Test: foldRight_nonDual")

	assert(traceFoldRight(list)(prod) == "(1 `prodOp` (10 `prodOp` (20 `prodOp` _)))",
		"Test 1: trace fold right")


	/**
	 * FoldRight and Unfold are Duals of each other:
	 *
	 * `def foldRight[E, B](init: List[E])(z: B)(op: (E, B) => B): B`
	 *
	 * `def unfold[E, A](init: A)(f: (A) => Option[(E, A)]): List[E]`
	 *
	 * The `foldRight` takes a list of `E` and produces a value while `unfold` does the opposite, takes a value and
	 * produces a list of `E`.
	 */

	def unfold_dual[E, A](init: A)(f: A => Option[(E, A)]): List[E] =
		f(init) match {
			case None => Nil
			case Some((e, a)) => e :: unfold_dual(a)(f)
		}



	assert(unfold_dual(10)(rangeOpt) == List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
		"Test: unfold dual type version")
	assert(traceUnfold(5)(rangeOpt) == List(List(), List(5), List(5, 4), List(5, 4, 3), List(5, 4, 3, 2), List(5, 4,
		3, 2, 1)),
		"Test: trace unfold dual version")

	/**
	 * --------------------------------------------------------------------------------------------------
	 * REFACTORING 0: Changing `foldRight` to show the duality of types better
	 *
	 * The type of the function passed to `foldRight` is `(E, B) ===> B`. We could think that `Either` represents
	 * this tuple as follows: `Either[(), (E, B)] ===> B`
	 *
	 * We could go further and replace `Either` with `Option` because it does not make sense to keep `Either` with a
	 * unit on the left, since `Either[Unit, A]` is isomorphic to `Option[A]`. Consequently, the next two
	 * definitions are equivalent:
	 *
	 * `def foldRight[E, B](init: List[E])(z: B)(op: (E, B) => B): B`
	 *
	 * `def foldRight[E, B](init: List[E])(f: Option[(E, B)] => B): B`
	 *
	 * NOTE: the `z` has not disappeared - it will be defined in the new function `f: Option[(E, B) => B]`
	 *
	 * Reimplementing `foldRight` to be a dual of `unfold`:
	 */
	def foldRight_dual[E, B](init: List[E])(f: Option[(E, B)] => B): B = init match {
		case Nil => f(None)
		case head :: tail => f(Some((head, foldRight_dual(tail)(f))))
	}


	assert(foldRight_dual(list)(prodOpt) == 5040, "Test: foldRight dual version")
	assert(list.foldRight(seed)(prod) == 5040, "Test: foldRight original on list (dual)")



	/**
	 * --------------------------------------------------------------------------------------------------
	 * REFACTORING 1: Pass Fewer Parameters
	 *
	 * First problem: In both cases, for `foldRight` and `unfold`, the initial values and function `f` are passed on
	 * every step of
	 * the recursion:
	 *
	 * `def foldRight[E, B](init: List[E])(f: Option[(E, B)] => B): B`
	 *
	 * `def unfold[E, A](init: A)(f: (A) => Option[(E, A)]): List[E]`
	 *
	 * Second problem:the parameters are curried; want to avoid that for better readability.
	 *
	 * Can get rid of both problems by (1) returning a function instead of a value, and (2) introducing a nested
	 * function. The type signatures look like this:
	 *
	 * `def foldRight[E, B](f: Option[(E, B)] => B): List[E] => B`
	 *
	 * `def unfold[E, A](f: A => Option[(E, A)]): A => List[E]`
	 *
	 * NOTE: the two signatures mirror each other:
	 *
	 * `Option[(E, B)] => B : 	 List[E] => B`
	 *
	 * `A => Option[(E, A)] :	 A => List[E]`
	 */

	def foldRight_fewParams[E, B](f: Option[(E, B)] => B): List[E] => B = {

		lazy val kernel: List[E] => B = _ match { // here matches on the list argument given
			case Nil => f(None)
			case head :: tail => f(Some((head, kernel(tail))))
		}

		kernel // foldRight_1 returns this function and thus takes a list argument (init: List[E])
	}

	assert(foldRight_fewParams(prodOpt)(list) == 5040, "Test: foldRight (few parameters)")
	assert(list.foldRight(seed)(prod) == 5040, "Test: foldRight original on list (few parameters)")


	def unfold_fewParams[E, A](f: A => Option[(E, A)]): A => List[E] = {

		lazy val kernel: A => List[E] = f(_) match { // matching on result of f(a)
			case None => Nil
			case Some((e, a)) => e :: kernel(a)
		}
		kernel // unfold_1 returns kernel function and thus takes an argument of type (init: A)
	}

	assert(unfold_fewParams(rangeOpt)(10) == List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), "Test: unfold (few parameters)")


	/**
	 * Using anonymous function instead of lazy val:
	 */
	def foldRight_anon[E, B](f: Option[(E, B)] => B): List[E] => B =

		new (List[E] => B) { kernel =>
			def apply(init: List[E]): B = init match {
				case Nil => f(None)
				case head :: tail => f(Some((head, kernel(tail))))
			}
		}

	//TESTING
	assert(foldRight_anon(prodOpt)(list) == 5040,
		"Test: foldRight (anonymous kernel function)")


	def unfold_anon[E, A](f: A => Option[(E, A)]): A => List[E] = {

		new (A => List[E]) { kernel =>
			def apply(init: A): List[E] = f(init) match {
				case None => Nil
				case Some((e, a)) => e :: kernel(a)
			}
		}
	}
	//TESTING
	assert(unfold_anon(rangeOpt)(10) == List(10,9,8,7,6,5,4,3,2,1),
		"Test: unfold (anonymous kernel function)")
}