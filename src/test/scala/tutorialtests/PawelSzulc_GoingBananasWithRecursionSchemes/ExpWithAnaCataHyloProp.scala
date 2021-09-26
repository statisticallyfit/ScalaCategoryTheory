package tutorialtests.PawelSzulc_GoingBananasWithRecursionSchemes


/**
 * Tutorial source = https://www.slideshare.net/paulszulc/going-bananas-with-recursion-schemes-for-fixed-point-data-types-72006242?next_slideshow=1
 */


//import org.scalatest.funspec.AnyFunSpec
/*import org.scalatest.matchers.should
import org.scalatest.{BeforeAndAfterAll/*, FlatSpec*/} //TODO where is FlatSpec? why not found?
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks*/


import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import scala.language.higherKinds


// NOTE: need this to avoid error of "No implicits found for Corecursive[T]" when doing anamorphism. MEANING: Fix needs to implement BirecursiveT typeclass
import matryoshka.data.Fix.birecursiveT
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra._
import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra.ExpOps._
import RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part5_AnaCoalgebra.ExpFunctor._


import scalaz.Functor
import scalaz.syntax.all._

/**
 *
 */

//trait A extends AnyFunSpec
/*
trait Spec extends AnyFunSpec with BeforeAndAfterAll with ScalaCheckDrivenPropertyChecks  with should.Matchers*/
// NOTE scalatestplus reference: https://github.com/subashprabanantham/hakuna-matata/blob/master/scalatest/src/test
// /scala/BaseScalaSpec.scala


class SpecClass extends Properties("Divisor Spec") /*with Spec*/ {


	trait DivisorLaws[F[_]] {

		implicit def functor: Functor[F]

		val positiveInt: Gen[Int] = arbitrary[Int] suchThat (_ > 0) // avoid division by zero error here?

		def divisorsToInitialValueByAnaThenCata = {
			property("ana then cata should evaluate divisors back to initial value") =
				forAll(positiveInt) { (number: Int) =>
					number.ana[Fix[Exp]](divisors).cata(evaluate) == number
				}
		}

		def divisorsToInitialValueByHylo = {
			property("hylo should evaluate divisors back to initial value") =
				forAll(positiveInt) { (number: Int) =>
					number.hylo[Exp, Double](evaluate, divisors) == number
				}
		}

		def divisorsToInitialValueEquivHyloAndAnaCata = {
			property("ana then cata is same as hylo for to evaluate divisors back to initial value") =
				forAll(positiveInt) { (number: Int) =>
					number.ana[Fix[Exp]](divisors).cata(evaluate) ==
						number.hylo[Exp, Double](evaluate, divisors)
				}
		}

		// TODO how to test catamorphism? (how to create arbitrary of Fix[Exp]? and then how to evaluate before the
		//  test to check the number the test results in?
	}

	object DivisorLaws {
		def apply[F[_]](implicit ev: Functor[F]): DivisorLaws[F] = new DivisorLaws[F] {
			def functor: Functor[F] = ev
		}
	}
	/*val divisorsToInitialValueByAnaThenCata = describe("divisors") {
		it("ana then cata should evaluate divisors back to initial value") {
			forAll(positiveInt) { (number: Int) =>
				number.ana[Fix[Exp]](divisors).cata(evaluate) shouldEqual number
			}
		}
	}*/

	/*describe("divisors"){
		/*it("ana then cata should evaluate divisors back to initial value") {
			val divisorsToInitialValueByAnaThenCata = forAll(positiveInt) { (number: Int) =>
				number.ana[Fix[Exp]](divisors).cata(evaluate) shouldEqual number
			}
		}*/
		it("hylo should evaluate divisors back to initial value") {
			val divisorsToInitialValueByHylo = forAll(positiveInt) { (number: Int) =>
				number.hylo[Exp, Double](evaluate, divisors) shouldEqual number
			}
		}
		it("ana then cata is same as hylo for to evaluate divisors back to initial value"){
			val divisorsToInitialValueEquivHyloAndAnaCata = forAll(positiveInt) { (number: Int) =>
				number.ana[Fix[Exp]](divisors).cata(evaluate) shouldEqual
					number.hylo[Exp, Double](evaluate, divisors)
			}
		}
	}*/

	/*val divisorsToInitialValueByHylo = describe("divisors") {
		it("hylo should evaluate divisors back to initial value") {
			forAll(positiveInt) { (number: Int) =>
				number.hylo[Exp, Double](evaluate, divisors) shouldEqual number
			}
		}
	}

	val divisorsToInitialValueEquivHyloAndAnaCata= describe("divisors") {
		it("ana then cata is same as hylo for to evaluate divisors back to initial value"){
			forAll(positiveInt) { (number: Int) =>

				number.ana[Fix[Exp]](divisors).cata(evaluate) shouldEqual
					number.hylo[Exp, Double](evaluate, divisors)
			}
		}
	}*/

}




object ExpWithAnaCataHyloProp extends SpecClass {

	DivisorLaws[Exp].divisorsToInitialValueByAnaThenCata
	DivisorLaws[Exp].divisorsToInitialValueByHylo
	DivisorLaws[Exp].divisorsToInitialValueEquivHyloAndAnaCata
}
