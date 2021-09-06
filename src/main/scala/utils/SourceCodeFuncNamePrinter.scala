package utils

/**
 *
 */
object SourceCodeFuncNamePrinter extends App {



	// TESTING sourcecode how to print function name

	val plusOne = (x: Int) => x + 1
	val minusOne = (x: Int) => x - 1

	def printer(fWithSrc: sourcecode.Text[Int => Int]) = {
		val f = fWithSrc.value
		println(s"Got this function ${ fWithSrc.source }. f(42) = ${ f(42) }")
	}

	printer(plusOne)
	printer(minusOne)
}
