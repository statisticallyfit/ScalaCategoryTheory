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

     checkAll("Either[Int, Int]",
          FunctorTests[Either[Int, ?]].functor[Int, Int, Int])

     /*def is =
          s2""" Either[Int, ?] forms a functor  $eitherFunctorProp
              Now this is something else $eitherFunctorProp1
            """*/

     /*def eitherFunctorProp = checkAll("Either[Int, Int]",
          FunctorTests[Either[Int, ?]].functor[Int, Int, Int])

     def eitherFunctorProp1 = checkAll("Either[Int, Int]",
          FunctorTests[Either[Int, ?]].functor[Int, Int, Int])*/
}
