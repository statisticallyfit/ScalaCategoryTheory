package RecursionSchemeTutorials.FortySevenDegrees



import scala.language.higherKinds
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

	// ----------------------------------------------------------------------------------------------

	//type ListF[A, B] = Option[(A, B)]

	//type ListF_F2[A, B] = Part2_FoldRefactorDataStructure.ListF[A, B]
	//type ListF_U2[A, B] = Part2_UnfoldRefactorDataStructure.ListF[A, B]

	val prodFList2: Part2_FoldRefactorDataStructure.ListF[Int, Int] => Int = { //type: F[B] => B
		_ match {
			case None => seed //z
			case Some((x, y)) => x * y
		}
	}

	val rangeFList2: Int => Part2_UnfoldRefactorDataStructure.ListF[Int, Int] = { // B => F[B]
		v => {
			if (v <= 0) None
			else Some((v, v - 1))
		}
	}


	// ----------------------------------------------------------------------------------------------

	//import Part3_CataAna.{Coalgebra, Algebra}


	val prodFList3: Part3_CataAna.ListF[Int, Int] => Int = { //type: F[B] => B
		_ match {
			case None => seed //z
			case Some((x, y)) => x * y
		}
	}

	val rangeFList3: Int => Part3_CataAna.ListF[Int, Int] = { // B => F[B]
		v => {
			if (v <= 0) None
			else Some((v, v - 1))
		}
	}


	val prodCataAlgebra1: Part3_CataAna.Algebra[Part3_CataAna.ListF[Int, ?], Int] = {
		case None => 1
		case Some((x, y)) => x * y
	}

	val rangeAnaCoalgebra1: Part3_CataAna.Coalgebra[Part3_CataAna.ListF[Int, ?], Int] = {
		v => {
			if (v <= 0) None
			else Some((v, v - 1))
		}
	}
}
