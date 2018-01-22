package functor.data


import cats.data.Validated._
import cats.data.Validated

import org.scalacheck.{Arbitrary, Gen}

/**
  *
  */
object ArbitraryADTs {

     implicit def arbIdentity[A: Arbitrary]: Arbitrary[Identity[A]] ={

          val genIdentity: Gen[Identity[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield Identity(a)

          Arbitrary(genIdentity)
     }


     implicit def arbPair[A: Arbitrary]: Arbitrary[Pair[A]] = {

          val genPair: Gen[Pair[A]] = for {
               a1 <- Arbitrary.arbitrary[A]
               a2 <- Arbitrary.arbitrary[A]
          }  yield Pair(a1, a2)

          Arbitrary(genPair)
     }

     implicit def arbTwo[A: Arbitrary, B: Arbitrary]: Arbitrary[Two[A, B]] ={

          val genTwo: Gen[Two[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield Two(a, b)

          Arbitrary(genTwo)
     }
}
