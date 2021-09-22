package categorytheorytests.monoid.prop


import categorytheorytests.monoid.data._
import categorytheorytests.monoid.data.ArbitraryADTs._
import categorytheorytests.myutil.Util

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._
import cats.data.Validated
import cats.{Eq, Monoid}
import cats.implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck._


/***
 *
 * A specification to test how different structures adhere to Monoid typeclass laws.
 *
 */
class Definitions extends Properties("Monoid") {


     trait MonoidAxioms[M] {
          implicit def M: Monoid[M]

          def associativeProperty(implicit t: TypeTag[M], c: ClassTag[M], e: Eq[M], a: Arbitrary[M]) = {

               property(s"${Util.inspect[M]}.associative: ((x |+| y) |+| z) === (x |+| (y |+| z))") = {
                    forAll { (x: M, y: M, z: M) =>
                         ((x |+| y) |+| z) === (x |+| (y |+| z))
                    }
               }
          }

          def leftIdentityProperty(implicit t: TypeTag[M], c: ClassTag[M], e: Eq[M], a: Arbitrary[M]) = {

               property(s"${Util.inspect[M]}.leftIdentity: (${M.empty} |+| x) === x") = {
                    forAll { (x: M) =>
                         (M.empty |+| x) === x
                    }
               }
          }

          def rightIdentityProperty(implicit t: TypeTag[M], c: ClassTag[M], e: Eq[M], a: Arbitrary[M]) = {

               property(s"${Util.inspect[M]}.rightIdentity: (x |+| ${M.empty}) === x") = {
                    forAll { (x: M) =>
                         (x |+| M.empty) === x
                    }
               }
          }
     }

     object MonoidAxioms {
          def apply[M](implicit ev: Monoid[M]): MonoidAxioms[M] = new MonoidAxioms[M] {
               def M: Monoid[M] = ev
          }
     }

}

object CustomMonoidProp extends Definitions {

     MonoidAxioms[Int].associativeProperty
     MonoidAxioms[Int].leftIdentityProperty
     MonoidAxioms[Int].rightIdentityProperty

     MonoidAxioms[String].associativeProperty
     MonoidAxioms[String].leftIdentityProperty
     MonoidAxioms[String].rightIdentityProperty

     MonoidAxioms[Set[Int]].associativeProperty
     MonoidAxioms[Set[Int]].leftIdentityProperty
     MonoidAxioms[Set[Int]].rightIdentityProperty

     MonoidAxioms[Option[List[Int]]].associativeProperty
     MonoidAxioms[Option[List[Int]]].leftIdentityProperty
     MonoidAxioms[Option[List[Int]]].rightIdentityProperty

     MonoidAxioms[Trivial].associativeProperty
     MonoidAxioms[Trivial].leftIdentityProperty
     MonoidAxioms[Trivial].rightIdentityProperty

     MonoidAxioms[Two[String, Int]].associativeProperty
     MonoidAxioms[Two[String, Int]].leftIdentityProperty
     MonoidAxioms[Two[String, Int]].rightIdentityProperty

     type Fivers = Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction]
     MonoidAxioms[Fivers].associativeProperty
     MonoidAxioms[Fivers].leftIdentityProperty
     MonoidAxioms[Fivers].rightIdentityProperty

     MonoidAxioms[Conjunction].associativeProperty
     MonoidAxioms[Conjunction].leftIdentityProperty
     MonoidAxioms[Conjunction].rightIdentityProperty

     MonoidAxioms[Disjunction].associativeProperty
     MonoidAxioms[Disjunction].leftIdentityProperty
     MonoidAxioms[Disjunction].rightIdentityProperty

     MonoidAxioms[ExclusiveDisjunction].associativeProperty
     MonoidAxioms[ExclusiveDisjunction].leftIdentityProperty
     MonoidAxioms[ExclusiveDisjunction].rightIdentityProperty

     MonoidAxioms[ExclusiveNorDisjunction].associativeProperty
     MonoidAxioms[ExclusiveNorDisjunction].leftIdentityProperty
     MonoidAxioms[ExclusiveNorDisjunction].rightIdentityProperty

     MonoidAxioms[Validated[String, Int]].associativeProperty
     MonoidAxioms[Validated[String, Int]].leftIdentityProperty
     MonoidAxioms[Validated[String, Int]].rightIdentityProperty

     MonoidAxioms[MyValidated[String, Int]].associativeProperty
     MonoidAxioms[MyValidated[String, Int]].leftIdentityProperty
     MonoidAxioms[MyValidated[String, Int]].rightIdentityProperty

     MonoidAxioms[AccumulateRight[String, Int]].associativeProperty
     MonoidAxioms[AccumulateRight[String, Int]].leftIdentityProperty
     MonoidAxioms[AccumulateRight[String, Int]].rightIdentityProperty

     MonoidAxioms[AccumulateBoth[String, List[String]]].associativeProperty
     MonoidAxioms[AccumulateBoth[String, List[String]]].leftIdentityProperty
     MonoidAxioms[AccumulateBoth[String, List[String]]].rightIdentityProperty


     //import monoid.data.Combine._

     import categorytheorytests.myutil.UnderlyingFunctionEq._


     MonoidAxioms[MyFunction[String, Int]].associativeProperty
     MonoidAxioms[MyFunction[String, Int]].leftIdentityProperty
     MonoidAxioms[MyFunction[String, Int]].rightIdentityProperty

     MonoidAxioms[Combine[String, Int]].associativeProperty
     MonoidAxioms[Combine[String, Int]].leftIdentityProperty
     MonoidAxioms[Combine[String, Int]].rightIdentityProperty

     MonoidAxioms[Memory[List[String], Int]].associativeProperty
     MonoidAxioms[Memory[List[String], Int]].leftIdentityProperty
     MonoidAxioms[Memory[List[String], Int]].rightIdentityProperty

}
