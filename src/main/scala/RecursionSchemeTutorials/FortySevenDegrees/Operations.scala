package RecursionSchemeTutorials.FortySevenDegrees

/**
 *
 */

object Operations {
	val seed: Int = 1


	val list: List[Int] = List(1,2,3,4,5,6,7)

	val prod: (Int, Int) => Int = _ * _

	val prodOpt: Option[(Int, Int)] => Int = {
		case None => seed // this is the `z` of the previous `foldRight` canonical definition
		case Some((x, y)) => x * y
	}



	// Example: generates  a list of descending integers starting with the given integer
	val rangeOpt: Int => Option[(Int, Int)] =
		v => {
			if (v <= 0) None
			else Some((v, v - 1))
		}



	import Part2_FoldRefactorDataStructure.ListF

	val prodFList: ListF[Int, Int] => Int = { //type: F[B] => B
		_ match {
			case None => seed //z
			case Some((x, y)) => x * y
		}
	}
}
