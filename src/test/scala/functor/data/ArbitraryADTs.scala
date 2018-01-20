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


}
