package RecursionSchemeTutorials.FortySevenDegrees



/**
 *
 */

object FoldUtils {

	//NOTE putting unfold here because the scala library doesn't HAVE unfold defined, not even for list, only for
	// scala version 2.13.4 and not all of my libraries are up to that lel.
	def unfold[E, A](init: A)(f: (A) => Option[(E, A)]): List[E] =
		f(init) match {
			case None => Nil
			case Some((e, a)) => e :: unfold(a)(f)
		}


	//TODO is this as abstract as the foldright trace? (need to write function name? currently not using sourcecode
	// here just applying the function like in unfold definition but wondering if should NOT evaluate and just show
	// the function name similar to foldright trace)
	def traceUnfold[E, A](init: A)(theFunction: sourcecode.Text[A => Option[(E, A)]]): List[List[E]] = {

		def loop(init: A)(f: A => Option[(E, A)])(acc: List[E]): List[List[E]] = {
			f(init) match {
				case None => acc :: Nil
				case Some((e, a)) => acc :: loop(a)(f)(acc :+ e)
			}
		}
		loop(init)(theFunction.value)(Nil)
	}


	def traceFoldRight[A,B](xs: List[A])(theFunction: sourcecode.Text[(A, B) => B]): String =
		xs.foldRight("_")((x, accStr) => s"($x `${theFunction.source}` $accStr)")

//	def traceUnfold[E, A](init: A)(theFunction: sourcecode.Text[A => Option[(E, A)]]): List[List[A]] =
//		unfold(init)()

}

object Part1_FoldBasic extends App {

	import FoldUtils._

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

	val prodOp: (Int, Int) => Int = _ * _

	val list: List[Int] = 1 :: 10 :: 20 :: Nil
	assert(foldRight_nonDual(list)(1)(prodOp) == 200, "Test: foldRight_nonDual")

	assert(traceFoldRight(list)(prodOp) == "(1 `prodOp` (10 `prodOp` (20 `prodOp` _)))",
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


	// Example: generates  a list of descending integers starting with the given integer
	val rangeOp: Int => Option[(Int, Int)] =
		v => {
			if (v <= 0) None
			else Some((v, v - 1))
		}

	assert(unfold_dual(10)(rangeOp) == List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
		"Test: unfold dual type version")
	assert(traceUnfold(5)(rangeOp) == List(List(), List(5), List(5, 4), List(5, 4, 3), List(5, 4, 3, 2), List(5, 4,
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

	val prodOp_fold: Option[(Int, Int)] => Int = {
		case None => 1 // this is the `z` of the previous `foldRight` canonical definition
		case Some((x, y)) => x * y
	}

	assert(foldRight_dual(1 :: 10 :: 20 :: Nil)(prodOp_fold) == 200, "Test: foldRight dual version")

	/*assert(traceFoldRight(list)(prodOp) == "(1 `prodOp` (10 `prodOp` (20 `prodOp` _)))",
		"Test 1: trace fold right")*/


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

	def foldRight_1[E, B](f: Option[(E, B)] => B): List[E] => B = {

		lazy val kernel: List[E] => B = _ match { // here matches on the list argument given
			case Nil => f(None)
			case head :: tail => f(Some((head, kernel(tail))))
		}

		kernel // foldRight_1 returns this function and thus takes a list argument (init: List[E])
	}

	assert(foldRight_1(prodOp_fold)(1 :: 10 :: 20 :: Nil) == 200, "Test: foldRight_1")


	def unfold_1[E, A](f: A => Option[(E, A)]): A => List[E] = {

		lazy val kernel: A => List[E] = f(_) match { // matching on result of f(a)
			case None => Nil
			case Some((e, a)) => e :: kernel(a)
		}
		kernel // unfold_1 returns kernel function and thus takes an argument of type (init: A)
	}

	assert(unfold_1(rangeOp)(10) == List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), "Test: unfold_1")


}