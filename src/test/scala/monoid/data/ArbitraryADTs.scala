package monoid.data

import cats.data.Validated._
import cats.data.Validated

import org.scalacheck.{Arbitrary, Gen}

/**
  *
  *
  */

object ArbitraryADTs {

     implicit def arbTrivial: Arbitrary[Trivial] = Arbitrary { Trivial() }

     implicit def arbTwo[A: Arbitrary, B: Arbitrary]: Arbitrary[Two[A, B]] ={
          //means this: //val genTwo: Gen[Two[A, B]] = Arbitrary.arbitrary[A].flatMap(a => Arbitrary.arbitrary[B].map(b => Two(a, b)))
          val genTwo: Gen[Two[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield Two(a, b)

          Arbitrary(genTwo)
     }

     implicit def arbFive[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary]: Arbitrary[Five[A, B, C, D, E]] ={

          val genFive: Gen[Five[A, B, C, D, E]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
               c <- Arbitrary.arbitrary[C]
               d <- Arbitrary.arbitrary[D]
               e <- Arbitrary.arbitrary[E]
          } yield Five(a, b, c, d, e)

          Arbitrary(genFive)
     }

     implicit def arbDisjunction(implicit ev: Arbitrary[Boolean]): Arbitrary[Disjunction] =
          Arbitrary { ev.arbitrary map { Disjunction(_) } }

     implicit def arbConjunction(implicit ev: Arbitrary[Boolean]): Arbitrary[Conjunction] =
          Arbitrary { ev.arbitrary map { Conjunction(_) } }

     implicit def arbExclDisjunction(implicit ev: Arbitrary[Boolean]): Arbitrary[ExclusiveDisjunction] =
          Arbitrary { ev.arbitrary map { ExclusiveDisjunction(_) } }

     implicit def arbExclNORDisjunction(implicit ev: Arbitrary[Boolean]): Arbitrary[ExclusiveNorDisjunction] =
          Arbitrary { ev.arbitrary map { ExclusiveNorDisjunction(_) } }


     implicit def arbValidation[E: Arbitrary, A: Arbitrary]: Arbitrary[Validated[E, A]] ={

          val genFailure: Gen[Invalid[E]] = for {
               err <- Arbitrary.arbitrary[E]
          } yield Invalid(err)

          val genSuccess: Gen[Valid[A]] = for {
               succ <- Arbitrary.arbitrary[A]
          } yield Valid(succ)

          val genValidated: Gen[Validated[E, A]] = Gen.oneOf(genFailure, genSuccess)

          Arbitrary(genValidated)
     }

     implicit def arbAccumulateRight[E: Arbitrary, A: Arbitrary]: Arbitrary[AccumulateRight[E, A]] ={

          val genAccumulateRight: Gen[AccumulateRight[E, A]] = for {
               validated <- Arbitrary.arbitrary[Validated[E, A]]
          } yield AccumulateRight(validated)

          Arbitrary(genAccumulateRight)
     }

     implicit def arbAccumulateBoth[E: Arbitrary, A: Arbitrary]: Arbitrary[AccumulateBoth[E, A]] ={

          val genAccumulateBoth: Gen[AccumulateBoth[E, A]] = for {
               validated <- Arbitrary.arbitrary[Validated[E, A]]
          } yield AccumulateBoth(validated)

          Arbitrary(genAccumulateBoth)
     }

     //Very similar to Choice in Functor ADTs
     implicit def arbMyValidated[E: Arbitrary, A: Arbitrary]: Arbitrary[MyValidated[E, A]] = {

          val genNotValid: Gen[NotValid[E]] = for {
               error <- Arbitrary.arbitrary[E]
          } yield NotValid(error)

          val genIsValid: Gen[IsValid[A]] = for {
               accept <- Arbitrary.arbitrary[A]
          } yield IsValid(accept)

          Arbitrary(Gen.oneOf(genNotValid, genIsValid))
     }


     implicit def arbFunction[A: Arbitrary, B:Arbitrary]: Arbitrary[MyFunction[A, B]] = {

          val genFunction: Gen[MyFunction[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield MyFunction((a:A) => b)

          Arbitrary(genFunction)
     }


     implicit def arbCombine[A: Arbitrary, B: Arbitrary]: Arbitrary[Combine[A, B]] ={

          val genCombine: Gen[Combine[A, B]] = for {
               a <- Arbitrary.arbitrary[A] //uncombine <- Arbitrary.arbitrary[A => B] /*arbFunction[A, B]*/
               b <- Arbitrary.arbitrary[B]
          } yield Combine((a:A) => b)

          Arbitrary(genCombine)
     }


     implicit def arbMemory[S: Arbitrary, A: Arbitrary]: Arbitrary[Memory[S, A]] ={

          val genMemory: Gen[Memory[S, A]] = for {
               s <- Arbitrary.arbitrary[S]
               a <- Arbitrary.arbitrary[A]
          } yield Memory((s:S) => (a, s))

          Arbitrary(genMemory)
     }
}