package RecursionSchemeTutorials.FortySevenDegrees

import sourcecode._
import io.functionmeta._
/**
 *
 */

object FoldUtils {

	def traceFoldRight[A,B](xs: List[A])(op: (A, B) => B): String =
		xs.foldRight("_")((x, accStr) => s"($x op $accStr)")

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
	 * Alternate FoldRight definition: (with List, to be dual of unfold)
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

	assert(traceFoldRight(list)(prodOp) == "(1 op (10 op (20 op _)))", "Test 1: trace fold right")
	//TODO find a way to get function name instead of just saying "op"

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

	//TODO testing sourcecode how to print function name
	val plusOne = (x: Int) => x + 1
	val minusOne = (x: Int) => x - 1

	def printer(fWithSrc: sourcecode.Text[Int => Int]) = {
		val f = fWithSrc.value
		println(s"Got this function ${ fWithSrc.source }. f(42) = ${ f(42) }")
	}

	printer(plusOne)
	printer(minusOne)

}
