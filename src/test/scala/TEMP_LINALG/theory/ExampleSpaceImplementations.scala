//package linalg.theory
//
//import linalg.numeric._
//import linalg.numeric.Number
//import linalg.numeric.Number._
//import linalg.theory._
//
//
//
//import cats.instances.int._
//import cats.instances.tuple._
//import cats.kernel.Eq
//import cats.laws.discipline._
//import org.scalacheck.Arbitrary.arbitrary
//import org.scalacheck.{Arbitrary, Gen}
//import org.scalatest.{FunSuite, Matchers}
//import org.typelevel.discipline.scalatest.Discipline
//
//
//
///**
//  * Concrete class examples are here.
//  */
//case class Polynomial[N: Number](coefs: N*)
//
//// ---------------------------------------------------------------------------------------
///**
//  * Concrete implementation of Space  typeclasses are here.
//  */
//object VectorSpace {
//
//}
//
//object InnerProductSpace{
//     implicit def PolyIsInner[N: Number] = new InnerProductSpace[Polynomial[N], N] {
//
//          val gen = implicitly[Number[N]]
//
//          val zero: Int => Polynomial[N] = n => Polynomial(List.fill[N](n)(gen.zero):_*)
//          val one: Int => Polynomial[N] = n => Polynomial(List.fill[N](n)(gen.one):_*)
//
//          def innerProduct(p: Polynomial[N], q: Polynomial[N]): N ={
//               p.coefs.zip(q.coefs).map{case (pc, qc) => pc* qc}.sum
//          }
//
//          def scale(p: Polynomial[N], factor: N): Polynomial[N] = Polynomial(p.coefs.map(_ * factor):_*)
//
//          def plus(p: Polynomial[N], q: Polynomial[N]): Polynomial[N] =
//               Polynomial(p.coefs.zip(q.coefs).map{case (pc, qc) => pc + qc}:_*)
//
//          def negate(p: Polynomial[N]): Polynomial[N] = Polynomial(p.coefs.map(_.negate()):_*)
//     }
//}
//
//
//class PolyInnerProductSpaceSpec extends FunSuite with Matchers with Discipline {
//
//     implicit def arbReal: Arbitrary[Real] ={
//
//          val genReal: Gen[Real] = for {
//               double <- Arbitrary.arbitrary[Double] //Gen.choose(Double.MinValue + 1, Double.MaxValue)
//          } yield Real(double)
//
//          Arbitrary(genReal)
//     }
//
//     implicit def arbRational: Arbitrary[Rational] ={
//
//          val genRational: Gen[Rational] = for {
//               n <- Arbitrary.arbitrary[Int] //Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)
//               d <- Arbitrary.arbitrary[Int] suchThat (_ != 0)
//          } yield Rational(n, d)
//
//          Arbitrary(genRational)
//     }
//
//     implicit def arbComplex[R: RealLike : Arbitrary]: Arbitrary[Complex[R]] ={
//
//          val genComplex: Gen[Complex[R]] = for {
//               realPart <- Arbitrary.arbitrary[R]
//               imagPart <- Arbitrary.arbitrary[R]
//          } yield Complex(realPart, imagPart)
//
//          Arbitrary(genComplex)
//     }
//
//     /*implicit def arbNumber[N : Number : Arbitrary, R: RealLike : Arbitrary]: Arbitrary[Number[N]] ={
//
//          /*val genReal: Gen[Real] = for {
//               double <- Arbitrary.arbitrary[Double] //Gen.choose(Double.MinValue + 1, Double.MaxValue)
//          } yield Real(double)
//
//          val genRational: Gen[Rational] = for {
//               n <- Arbitrary.arbitrary[Int] //Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)
//               d <- Arbitrary.arbitrary[Int] suchThat (_ != 0)
//          } yield Rational(n, d)
//
//          val genComplex: Gen[Complex[R]] = for {
//               realPart <- Arbitrary.arbitrary[R]
//               imagPart <- Arbitrary.arbitrary[R]
//          } yield Complex(realPart, imagPart)*/
//
//          val genNumber: Gen[N] = Gen.oneOf(arbReal, arbRational)
//
//          Arbitrary(genNumber)
//     }*/
//
//     implicit def arbitraryPolynomial[N: Number : Arbitrary]: Arbitrary[Polynomial[N]] ={
//
//          val genPoly: Gen[Polynomial[N]] = for {
//               length <- Gen.choose(1, 10)
//               n <- Arbitrary.arbitrary[List[N]]
//
//               //todo check here for Gen.listOfN: https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md
//          }
//
//          //Arbitrary(Gen.frequency((1, Polynomial(Gen.frequency((10, arbitrary[N].map(n => n)))))))
//     }
//}