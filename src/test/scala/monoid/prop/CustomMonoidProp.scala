package monoid.prop


import monoid.data._
import monoid.data.ArbitraryADTs._
import Util.Util

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
object CustomMonoidProp extends Properties("Monoid")  {


     trait MonoidAxioms[M] {
          implicit def M: Monoid[M]

          def associativeProperty(implicit t: TypeTag[M], c: ClassTag[M], e: Eq[M], a: Arbitrary[M]) ={

               property(s"${Util.inspect[M]}.associative: ((x |+| y) |+| z) === (x |+| (y |+| z))") = {
                    forAll {(x: M, y: M, z: M) =>
                         ((x |+| y) |+| z) === (x |+| (y |+| z))
                    }
               }
          }

          def leftIdentityProperty(implicit t: TypeTag[M], c: ClassTag[M], e: Eq[M], a: Arbitrary[M]) ={

               property(s"${Util.inspect[M]}.leftIdentity: (${M.empty} |+| x) === x") = {
                    forAll {(x: M) =>
                         (M.empty |+| x) === x
                    }
               }
          }

          def rightIdentityProperty(implicit t: TypeTag[M], c: ClassTag[M], e: Eq[M], a: Arbitrary[M]) ={
               //val monoid = implicitly[Monoid[M]]

               property(s"${Util.inspect[M]}.rightIdentity: (x |+| ${M.empty}) === x") = {
                    forAll {(x: M) =>
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
     /*object MonoidAxioms {

          def associativeProperty[M: Monoid : Eq : Arbitrary](implicit t: TypeTag[M], c: ClassTag[M]) ={

               property(s"${Util.inspect[M]}.associative: ((x |+| y) |+| z) === (x |+| (y |+| z))") = {
                    forAll {(x: M, y: M, z: M) =>
                         ((x |+| y) |+| z) === (x |+| (y |+| z))
                    }
               }
          }

          def leftIdentityProperty[M : Monoid: Eq : Arbitrary](implicit t: TypeTag[M], c: ClassTag[M]) ={
               val monoid = implicitly[Monoid[M]]

               property(s"${Util.inspect[M]}.leftIdentity: (${monoid.empty} |+| x) === x") = {
                    forAll {(x: M) =>
                         (monoid.empty |+| x) === x
                    }
               }
          }

          def rightIdentityProperty[M : Monoid: Eq : Arbitrary](implicit t: TypeTag[M], c: ClassTag[M]) ={
               val monoid = implicitly[Monoid[M]]

               property(s"${Util.inspect[M]}.rightIdentity: (x |+| ${monoid.empty}) === x") = {
                    forAll {(x: M) =>
                         (x |+| monoid.empty) === x
                    }
               }
          }
     }*/

     //Tests begin


     MonoidAxioms[Int].associativeProperty
     MonoidAxioms[Int].leftIdentityProperty
     MonoidAxioms[Int].rightIdentityProperty

     /*MonoidAxioms.associativeProperty[Int]
     MonoidAxioms.leftIdentityProperty[Int]
     MonoidAxioms.rightIdentityProperty[Int]*/

     /*MonoidAxioms.associativeProperty[String]
     MonoidAxioms.leftIdentityProperty[String]
     MonoidAxioms.rightIdentityProperty[String]

     MonoidAxioms.associativeProperty[Set[Int]]
     MonoidAxioms.leftIdentityProperty[Set[Int]]
     MonoidAxioms.rightIdentityProperty[Set[Int]]

     MonoidAxioms.associativeProperty[Trivial]
     MonoidAxioms.leftIdentityProperty[Trivial]
     MonoidAxioms.rightIdentityProperty[Trivial]

     MonoidAxioms.associativeProperty[Disjunction]
     MonoidAxioms.leftIdentityProperty[Disjunction]
     MonoidAxioms.rightIdentityProperty[Disjunction]

     MonoidAxioms.associativeProperty[Conjunction]
     MonoidAxioms.leftIdentityProperty[Conjunction]
     MonoidAxioms.rightIdentityProperty[Conjunction]

     MonoidAxioms.associativeProperty[ExclusiveDisjunction]
     MonoidAxioms.leftIdentityProperty[ExclusiveDisjunction]
     MonoidAxioms.rightIdentityProperty[ExclusiveDisjunction]

     MonoidAxioms.associativeProperty[ExclusiveNorDisjunction]
     MonoidAxioms.leftIdentityProperty[ExclusiveNorDisjunction]
     MonoidAxioms.rightIdentityProperty[ExclusiveNorDisjunction]

     MonoidAxioms.associativeProperty[Two[String, Int]]
     MonoidAxioms.leftIdentityProperty[Two[String, Int]]
     MonoidAxioms.rightIdentityProperty[Two[String, Int]]

     MonoidAxioms.associativeProperty[Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction]]
     MonoidAxioms.leftIdentityProperty[Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction]]
     MonoidAxioms.rightIdentityProperty[Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction]]

     MonoidAxioms.associativeProperty[Option[List[Int]]]
     MonoidAxioms.leftIdentityProperty[Option[List[Int]]]
     MonoidAxioms.rightIdentityProperty[Option[List[Int]]]

     MonoidAxioms.associativeProperty[Either[String, Int]]
     MonoidAxioms.leftIdentityProperty[Either[String, Int]]
     MonoidAxioms.rightIdentityProperty[Either[String, Int]]

     MonoidAxioms.associativeProperty[Validated[String, Int]]
     MonoidAxioms.leftIdentityProperty[Validated[String, Int]]
     MonoidAxioms.rightIdentityProperty[Validated[String, Int]]

     MonoidAxioms.associativeProperty[AccumulateRight[String, Int]]
     MonoidAxioms.leftIdentityProperty[AccumulateRight[String, Int]]
     MonoidAxioms.rightIdentityProperty[AccumulateRight[String, Int]]

     MonoidAxioms.associativeProperty[AccumulateBoth[String, List[String]]]
     MonoidAxioms.leftIdentityProperty[AccumulateBoth[String, List[String]]]
     MonoidAxioms.rightIdentityProperty[AccumulateBoth[String, List[String]]]


     import monoid.data.Combine._
     import monoid.data.FunctionEq._

     MonoidAxioms.associativeProperty[Combine[String, Int]]
     MonoidAxioms.leftIdentityProperty[Combine[String, Int]]
     MonoidAxioms.rightIdentityProperty[Combine[String, Int]]

     MonoidAxioms.associativeProperty[Memory[List[String], Int]]
     MonoidAxioms.leftIdentityProperty[Memory[List[String], Int]]
     MonoidAxioms.rightIdentityProperty[Memory[List[String], Int]]*/
}
