package tutorialtests.PawelSzulc_GoingBananasWithRecursionSchemes

/**
 *
 */
//import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
//import org.specs2.mutable._
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalatest._

import org.scalatest.prop._
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers._
import org.scalatest.{BeforeAndAfterAll/*, FlatSpec*/} //TODO where is FlatSpec? why not found?

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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

/**
 *
 */

//trait A extends AnyFunSpec

trait Spec extends AnyFunSpec with /*BeforeAndAfterAll with*/ ScalaCheckDrivenPropertyChecks  with should.Matchers

class DivisorLaws extends Properties("Divisor Spec") with Spec {

	val positiveInt: Gen[Int] = arbitrary[Int] suchThat (_ > 0) // avoid division by zero error here?

	val divisorsToInitialValueByAnaThenCata = forAll(positiveInt) { (number: Int) =>
		number.ana[Fix[Exp]](divisors).cata(evaluate) shouldEqual number
	}
	/*val divisorsToInitialValueByAnaThenCata = describe("divisors") {
		it("ana then cata should evaluate divisors back to initial value") {
			forAll(positiveInt) { (number: Int) =>
				number.ana[Fix[Exp]](divisors).cata(evaluate) shouldEqual number
			}
		}
	}*/

	val divisorsToInitialValueByHylo = describe("divisors") {
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
	}

}




object ExpWithAnaCataHyloProp  {

	val d = new DivisorLaws
	//d.divisorsToInitialValueByAnaThenCata.check
	// TODO how to call these things? want to make it like those in functortests but cannot since apply method
	//  doesn't take those kinds of parameters

	/*DivisorLaws.divisorsToInitialValueByAnaThenCata
	DivisorLaws.divisorsToInitialValueByHylo
	DivisorLaws.divisorsToInitialValueEquivHyloAndAnaCata*/
}
