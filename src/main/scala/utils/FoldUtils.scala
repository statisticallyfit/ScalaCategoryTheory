package utils

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


}