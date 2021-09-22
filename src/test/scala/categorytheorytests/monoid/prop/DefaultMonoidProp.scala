package categorytheorytests.monoid.prop


import categorytheorytests.monoid.data._
import categorytheorytests.monoid.data.ArbitraryADTs._

import cats.data.Validated
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.kernel.laws.MonoidLaws
import spire.laws.GroupLaws
import org.specs2.SpecificationLike
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.Discipline

/**
  *
  */


trait CatsSpecMonoid extends Specification with Discipline with SpecificationLike with AllInstances with AllSyntax

//todo - how to separate the output ? No titles or anything ...


class DefaultMonoidProp extends CatsSpecMonoid {

     checkAll("Monoid[Int]", GroupLaws[Int].monoid)

     checkAll("Monoid[String]", GroupLaws[String].monoid) //using spire laws

     checkAll("Monoid[Set[Int]]", GroupLaws[Set[Int]].monoid)

     checkAll("Monoid[String]", GroupLaws[Option[List[Int]]].monoid)

     checkAll("Monoid[String]", GroupLaws[Either[String, Int]].monoid)

     checkAll("Monoid[Trivial]", GroupLaws[Trivial].monoid)

     checkAll("Monoid[Two[String, Int]]", GroupLaws[Two[String, Int]].monoid)

     checkAll("Monoid[Five[Trivial, Conjunction, Disjunction, " +
          "ExclusiveDisjunction, ExclusiveNorDisjunction]]",
          GroupLaws[Five[Trivial, Conjunction, Disjunction,
               ExclusiveDisjunction, ExclusiveNorDisjunction]].monoid)

     checkAll("Monoid[Disjunction]", GroupLaws[Disjunction].monoid)

     checkAll("Monoid[Conjunction]", GroupLaws[Conjunction].monoid)

     checkAll("Monoid[ExclusiveDisjunction]", GroupLaws[ExclusiveDisjunction].monoid)

     checkAll("Monoid[ExclusiveNorDisjunction]", GroupLaws[ExclusiveNorDisjunction].monoid)

     checkAll("Monoid[Validated[String, Int]]", GroupLaws[Validated[String, Int]].monoid)

     checkAll("Monoid[MyValidated[String, Int]]", GroupLaws[MyValidated[String, Int]].monoid)

     checkAll("Monoid[AccumulateRight[String, Int]]", GroupLaws[AccumulateRight[String, Int]].monoid)

     checkAll("Monoid[AccumulateBoth[String, List[String]]]", GroupLaws[AccumulateBoth[String, List[String]]].monoid)


     //import monoid.data.Combine._
     import categorytheorytests.myutil.UnderlyingFunctionEq._

     checkAll("Monoid[MyFunction[Int, String]]", GroupLaws[MyFunction[Int, String]].monoid)

     checkAll("Monoid[Combine[String, Int]]", GroupLaws[Combine[String, Int]].monoid)

     checkAll("Monoid[Memory[List[String], Int]]", GroupLaws[Memory[List[String], Int]].monoid)




}
