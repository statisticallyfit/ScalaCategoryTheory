package functor.prop



import functor.data._
import functor.data.ArbitraryADTs._

import cats.data.Validated
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import cats.laws.discipline.FunctorTests

import org.specs2.SpecificationLike
import org.specs2.mutable._
import org.typelevel.discipline.specs2.Discipline


/**
  *
  */

trait CatsSpec extends Specification with Discipline with SpecificationLike with AllInstances with AllSyntax

class DefaultFunctorProp extends CatsSpec {

     checkAll("Either[Int, Int]",  FunctorTests[Either[Int, ?]].functor[Int, Int, Int])

     //checkAll("Validated[String, Int]", FunctorTests[Validated[String, ?]].functor[Int, Double, String])

     checkAll("Identity[Int]", FunctorTests[Identity].functor[Int, Int, String])

     checkAll("Pair[Int]", FunctorTests[Pair].functor[Int, Int, String])

     checkAll("Two[String, Int]", FunctorTests[Two[String, ?]].functor[Int, Double, String])

     checkAll("Three[Int, String, Double]", FunctorTests[Three[Int, String, ?]].functor[Double, Int, Int])

     checkAll("Sum[Identity[Int], String]", FunctorTests[Sum[Identity[Int], ?]].functor[String, Int, Int])

     checkAll("Quant[Double, String]", FunctorTests[Quant[Double, ?]].functor[String, Double, String])

     checkAll("Maybe[Int]", FunctorTests[Maybe].functor[Int, Int, Int])

     checkAll("Company[Int, String, Int]", FunctorTests[Company[Int, String, ?]].functor[Int, Int, Int])

     checkAll("TalkToMe[Int]", FunctorTests[TalkToMe].functor[Int, Int, Int])

     checkAll("BinaryTree[Int]", FunctorTests[BinaryTree].functor[Int, Int, Int])

     checkAll("Train", FunctorTests[Train].functor[Double, Double, Double])

     checkAll("Konstant[Double, Int]", FunctorTests[Konstant[Double, ?]].functor[Int, Int, Int])

     checkAll("OtherKonstant[Double, Int]", FunctorTests[OtherKonstant[Double, ?]].functor[Int, Int, Int])

     checkAll("LiftItOut[Int, Int]", FunctorTests[LiftItOut[Int, ?]].functor[Int, Int, Int])

     checkAll("Together[Int, Int]", FunctorTests[Together[Int, ?]].functor[Int, Int, Int])

     checkAll("Separate[Int, Int, Int, Int]", FunctorTests[Separate[Int, Int, Int, ?]].functor[Int, Int, Int])

     checkAll("Notorious[Int, Double, String, Int, Int, ?]",
          FunctorTests[Notorious[Int, Double, String, Int, Int, ?]].functor[Int, Int, Int])

}
