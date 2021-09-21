package tutorialtests.PawelSzulc_GoingBananasWithRecursionSchemes

/**
 *
 */
//import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
//import org.specs2.mutable._
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._
import org.scalatest.propspec.AnyPropSpec

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.arbitrary


import scala.language.higherKinds
import matryoshka._
// NOTE: need this to avoid error of "No implicits found for Corecursive[T]" when doing anamorphism. MEANING: Fix needs to implement BirecursiveT typeclass
import matryoshka.data.Fix.birecursiveT
import matryoshka.data._
import matryoshka.implicits._


import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra._
import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra.ExpOps._
import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra.ExpFunctor._


// FlatSpec with Matchers with BeforeAndAfterAll
/*trait BaseScalaSpec extends Specification with ScalaCheckDrivenPropertyChecks {

}*/

class DivisorLaws extends Properties("Divisor Spec") {



	trait DivisorLaws[A]  {
		val positiveInt: Gen[Int] = arbitrary[Int] suchThat (_ > 0) // avoid division by zero error here?

		val divisorsToInitialValueByAnaThenCata = {
			property("ana then cata should evaluate divisors back to initial value") =
				forAll(positiveInt) { (number: Int) =>
					number.ana[Fix[Exp]](divisors).cata(evaluate) == number
				}
		}

		val divisorsToInitialValueByHylo = {
			property("hylo should evaluate divisors back to initial value") =
				forAll(positiveInt) { (number: Int) =>
					number.hylo[Exp, Double](evaluate, divisors) == number
				}
		}

		val divisorsToInitialValueEquivHyloAndAnaCata = {
			property("ana then cata is same as hylo for to evaluate divisors back to initial value") =
				forAll(positiveInt) { (number: Int) =>

					number.ana[Fix[Exp]](divisors).cata(evaluate) ==
						number.hylo[Exp, Double](evaluate, divisors)
				}
		}
	}

// TODO HELP left off here but cannot instantiate the divisorlaws because of no object but how else to define apply? 
	/*object DivisorLaws {
		def apply[A](implicit ev: Arbitrary[A]): DivisorLaws[A] = new DivisorLaws[A]{
			override val positiveInt: Gen[Int] = ev
		}
	}*/
}
/*
class DivisorHyloTests extends Properties("Hylo") /*with ScalaCheckPropertyChecks*/ with should.Matchers */


object ExpWithAnaCataHyloProp extends DivisorLaws {

	DivisorLaws[Int].divisorsToInitialValueByAnaThenCata
}
