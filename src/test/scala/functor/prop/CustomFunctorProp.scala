package functor.prop


import functor.data._
import functor.data.ArbitraryADTs._
import Util.Util


import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._
import scala.language.higherKinds


import cats.{Eq, Functor}
import cats.implicits._


import org.scalacheck.Prop.forAll
import org.scalacheck._



/***
  *
  * A specification to test how different structures adhere to Functor typeclass laws.
  *
  */

class Definitions extends Properties("Functor") {

     trait FunctorAxioms[F[_]] {

          implicit def F : Functor[F]  //this makes map, identity available in scope

          def identityProperty[A : Eq : Arbitrary](implicit tf: TypeTag[F[A]],
                                                   cf: ClassTag[F[A]],
                                                   fEq: Eq[F[A]],
                                                   fArb: Arbitrary[F[A]]): ListBuffer[(String, Prop)] = {

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
}


object CustomFunctorProp extends Definitions {

     FunctorAxioms[Identity].identityProperty[Int]
     FunctorAxioms[Identity].compositionProperty[Int, Int, Int]
     FunctorAxioms[Identity].compositionProperty[Int, String, List[Int]]

     FunctorAxioms[Pair].identityProperty[String]
     FunctorAxioms[Pair].compositionProperty[String, Int, Int]
     FunctorAxioms[Pair].compositionProperty[String, Option[Int], String]

     FunctorAxioms[Two[String, ?]].identityProperty[Int]
     FunctorAxioms[Two[String, ?]].compositionProperty[Int, String, Either[String, Int]]

     FunctorAxioms[Three[Int, String, ?]].identityProperty[Int]
     FunctorAxioms[Three[Int, String, ?]].compositionProperty[Int, Option[Int], Boolean]

     FunctorAxioms[Sum[Identity[Int], ?]].identityProperty[String]
     FunctorAxioms[Sum[Identity[Int], ?]].compositionProperty[String, List[Int], Int]

     FunctorAxioms[Quant[Double, ?]].identityProperty[Int]
     FunctorAxioms[Quant[Double, ?]].compositionProperty[Int, Boolean, Int]

     FunctorAxioms[Maybe].identityProperty[String]
     FunctorAxioms[Maybe].compositionProperty[String, Option[String], String]

     FunctorAxioms[Company[Int, String, ?]].identityProperty[Double]
     FunctorAxioms[Company[Int, String, ?]].compositionProperty[Double, Int, Double]

     FunctorAxioms[Decision[(Int, Int), ?]].identityProperty[Int]
     FunctorAxioms[Decision[(Int, Int), ?]].compositionProperty[Int, String, List[Int]]

     FunctorAxioms[TalkToMe].identityProperty[Int]
     FunctorAxioms[TalkToMe].compositionProperty[Int, String, String]

     FunctorAxioms[BinaryTree].identityProperty[Int]
     FunctorAxioms[BinaryTree].compositionProperty[Int, Int, Int]

     FunctorAxioms[Train].identityProperty[Double]
     FunctorAxioms[Train].compositionProperty[Double, Double, Double]

     FunctorAxioms[ConstA[Double, ?]].identityProperty[Int]
     FunctorAxioms[ConstA[Double, ?]].compositionProperty[Int, Double, Int]

     FunctorAxioms[ConstB[Double, ?]].identityProperty[Int]
     FunctorAxioms[ConstB[Double, ?]].compositionProperty[Int, Double, Int]

     FunctorAxioms[LiftItOut[Int, ?]].identityProperty[Int]
     FunctorAxioms[LiftItOut[Int, ?]].compositionProperty[Int, Double, Int]

     FunctorAxioms[Together[Int, ?]].identityProperty[Int]
     FunctorAxioms[Together[Int, ?]].compositionProperty[Int, Double, Int]

     FunctorAxioms[Separate[Int, String, Double, ?]].identityProperty[Int]
     FunctorAxioms[Separate[Int, String, Double, ?]].compositionProperty[Int, Double, Int]

     FunctorAxioms[Notorious[Int, Double, String, Int, Int, ?]].identityProperty[Int]
     FunctorAxioms[Notorious[Int, Double, String, Int, Int, ?]].compositionProperty[Int, Double, Int]
}


