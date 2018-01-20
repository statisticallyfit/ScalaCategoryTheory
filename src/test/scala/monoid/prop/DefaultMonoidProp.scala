package monoid.prop


import monoid.data._
import monoid.data.ArbitraryADTs._

import cats.data.Validated
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import cats.kernel.laws.discipline.MonoidTests
import cats.kernel.laws.MonoidLaws
import spire.laws.GroupLaws

import org.specs2.SpecificationLike
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.Discipline

/**
  *
  */


trait CatsSpec extends Specification with Discipline with SpecificationLike with AllInstances with AllSyntax

//todo - how to separate the output ? No titles or anything ...

class DefaultMonoidProp extends CatsSpec {

     checkAll("Monoid[Int]", GroupLaws[Int].monoid)
     //checkAll("Monoid[Int]", MonoidTests[Int].monoid)

     checkAll("Monoid[String]", GroupLaws[String].monoid) //using spire laws
     //checkAll("Monoid[String]", MonoidTests[String].monoid) //using cats kernel laws

     checkAll("Monoid[Set[Int]]", GroupLaws[Set[Int]].monoid)
     //checkAll("Monoid[Set[Int]]", MonoidTests[Set[Int]].monoid)

     checkAll("Monoid[Trivial]", GroupLaws[Trivial].monoid)
     //checkAll("Monoid[Trivial]", MonoidTests[Trivial].monoid)

     checkAll("Monoid[Disjunction]", GroupLaws[Disjunction].monoid)
     checkAll("Monoid[Disjunction]", MonoidTests[Disjunction].monoid)

     checkAll("Monoid[Conjunction]", GroupLaws[Conjunction].monoid)
     //checkAll("Monoid[Conjunction]", MonoidTests[Conjunction].monoid)

     checkAll("Monoid[ExclusiveDisjunction]", GroupLaws[ExclusiveDisjunction].monoid)
     //checkAll("Monoid[ExclusiveDisjunction]", MonoidTests[ExclusiveDisjunction].monoid)

     checkAll("Monoid[ExclusiveNorDisjunction]", GroupLaws[ExclusiveNorDisjunction].monoid)
     //checkAll("Monoid[ExclusiveNorDisjunction]", MonoidTests[ExclusiveNorDisjunction].monoid)

     checkAll("Monoid[Two[String, Int]]", GroupLaws[Two[String, Int]].monoid)
     //checkAll("Monoid[Two[String, Int]]", MonoidTests[Two[String, Int]].monoid)


     checkAll("Monoid[Five[Trivial, Conjunction, Disjunction, " +
          "ExclusiveDisjunction, ExclusiveNorDisjunction]]",
          GroupLaws[Five[Trivial, Conjunction, Disjunction,
               ExclusiveDisjunction, ExclusiveNorDisjunction]].monoid)

     /*checkAll("Monoid[Five[Trivial, Conjunction, Disjunction, " +
          "ExclusiveDisjunction, ExclusiveNorDisjunction]]",
          MonoidTests[Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction]].monoid)*/


     checkAll("Monoid[String]", GroupLaws[Option[List[Int]]].monoid)
     //checkAll("Monoid[String]", MonoidTests[Option[List[Int]]].monoid)

     checkAll("Monoid[String]", GroupLaws[Either[String, Int]].monoid)
     //checkAll("Monoid[String]", MonoidTests[Either[String, Int]].monoid)

     checkAll("Monoid[Validated[String, Int]]", GroupLaws[Validated[String, Int]].monoid)
     //checkAll("Monoid[Validated[String, Int]]", MonoidTests[Validated[String, Int]].monoid)

     checkAll("Monoid[AccumulateRight[String, Int]]", GroupLaws[AccumulateRight[String, Int]].monoid)
     //checkAll("Monoid[AccumulateRight[String, Int]]", MonoidTests[AccumulateRight[String, Int]].monoid)

     checkAll("Monoid[AccumulateBoth[String, List[String]]]", GroupLaws[AccumulateBoth[String, List[String]]].monoid)
     //checkAll("Monoid[AccumulateBoth[String, List[String]]]", MonoidTests[AccumulateBoth[String, List[String]]]
     // .monoid)

     import monoid.data.Combine._
     import monoid.data.FunctionEq._
     checkAll("Monoid[Combine[String, Int]]", GroupLaws[Combine[String, Int]].monoid)
     //checkAll("Monoid[Combine[String, Int]]", MonoidTests[Combine[String, Int]].monoid)

     checkAll("Monoid[Memory[List[String], Int]]", GroupLaws[Memory[List[String], Int]].monoid)
     //checkAll("Monoid[Memory[List[String], Int]]", MonoidTests[Memory[List[String], Int]].monoid)

}
