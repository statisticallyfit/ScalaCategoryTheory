package functor.prop


import functor.data._
import functor.data.ArbitraryADTs._
import Util.Util

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._
import scala.language.higherKinds

import cats.data.Validated
import cats.{Eq, Monoid, Functor}
import cats.implicits._


import org.scalacheck.Prop.forAll
import org.scalacheck._


/***
  *
  * A specification to test how different structures adhere to Functor typeclass laws.
  *
  */
object CustomFunctorProp  extends Properties("Functor")  {


     trait FunctorAxioms[F[_]] {

          implicit def F : Functor[F]  //this makes map, identity available in scope

          def identityProperty[A : Eq : Arbitrary](implicit tf: TypeTag[F[A]],
                                                   cf: ClassTag[F[A]],
                                                   fEq: Eq[F[A]],
                                                   fArb: Arbitrary[F[A]]) = {

               val typeName = s"${Util.inspect[F[A]]}"

               property(s"$typeName.identity: fa.map(identity) === fa") ={
                    forAll {(fa: F[A]) =>
                         fa.map(identity) === fa
                    }
               }
          }

          def compositionProperty[A :Eq:Arbitrary, B :Eq:Arbitrary, C :Eq:Arbitrary]
               (implicit tf: TypeTag[F[A]], cf: ClassTag[F[A]], eqFC: Eq[F[C]], arbFA: Arbitrary[F[A]],
                arbAtoB: Arbitrary[A => B], arbBtoC: Arbitrary[B => C]) = {

               val typeName = s"${Util.inspect[F[A]]}"

               property(s"$typeName.composition: fa.map(f).map(g) === fa.map(f andThen g)") ={
                    forAll {(fa: F[A], f: A => B, g: B => C) =>
                         fa.map(f).map(g) === fa.map(f andThen g)
                    }
               }
          }
     }

     object FunctorAxioms {
          def apply[F[_]](implicit ev: Functor[F]): FunctorAxioms[F] = new FunctorAxioms[F] {
                    def F: Functor[F] = ev
          }
     }



     //Tests begin:

     FunctorAxioms[Identity].identityProperty[Int]
     FunctorAxioms[Identity].compositionProperty[Int, Int, Int]
     FunctorAxioms[Identity].compositionProperty[Int, String, List[Int]]

     FunctorAxioms[Pair].identityProperty[String]
     FunctorAxioms[Pair].compositionProperty[Int, Int, Int]
     FunctorAxioms[Pair].compositionProperty[Int, Int, String]


}
