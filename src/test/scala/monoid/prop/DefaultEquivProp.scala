/*
package monoid.prop

import monoid.data._
import monoid.data.ArbitraryADTs._
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


trait CatsSpecEqv extends Specification with Discipline with SpecificationLike with AllInstances with AllSyntax



// Preliminary equality checks for the Monoid, since ADTs depend on equality
class DefaultEquivProp extends CatsSpecMonoid {

	checkAll("Monoid[Int]", EqTests[Int].eqv)


	checkAll("Monoid[String]", EqTests[String].eqv) //using spire laws

	checkAll("Monoid[Set[Int]]", EqTests[Set[Int]].eqv)

	checkAll("Monoid[Trivial]", EqTests[Trivial].eqv)

	checkAll("Monoid[Disjunction]", EqTests[Disjunction].eqv)

	checkAll("Monoid[Conjunction]", EqTests[Conjunction].eqv)

	checkAll("Monoid[ExclusiveDisjunction]", EqTests[ExclusiveDisjunction].eqv)

	checkAll("Monoid[ExclusiveNorDisjunction]", EqTests[ExclusiveNorDisjunction].eqv)

	checkAll("Monoid[Two[String, Int]]", EqTests[Two[String, Int]].eqv)


	checkAll("Monoid[Five[Trivial, Conjunction, Disjunction, " +
		"ExclusiveDisjunction, ExclusiveNorDisjunction]]",
		EqTests[Five[Trivial, Conjunction, Disjunction,
			ExclusiveDisjunction, ExclusiveNorDisjunction]].eqv)


	checkAll("Monoid[String]", EqTests[Option[List[Int]]].eqv)

	checkAll("Monoid[String]", EqTests[Either[String, Int]].eqv)

	checkAll("Monoid[Validated[String, Int]]", EqTests[Validated[String, Int]].eqv)

	checkAll("Monoid[AccumulateRight[String, Int]]", EqTests[AccumulateRight[String, Int]].eqv)

	checkAll("Monoid[AccumulateBoth[String, List[String]]]", EqTests[AccumulateBoth[String, List[String]]].eqv)

	import monoid.data.Combine._
	import monoid.data.FunctionEq._
	checkAll("Monoid[Combine[String, Int]]", EqTests[Combine[String, Int]].eqv)

	checkAll("Monoid[Memory[List[String], Int]]", EqTests[Memory[List[String], Int]].eqv)
}*/
