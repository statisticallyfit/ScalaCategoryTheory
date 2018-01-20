package functor.spec


import functor.data._

import cats.Functor
import cats.data.Validated.{Valid, Invalid}
import cats.implicits._

import org.specs2.mutable._

/**
  *
  */

class FunctorSpec extends Specification {

     /*"Monoid is a typeclass that can be used to combine things of the same kind" should {

          "-> (Int, +) is a Monoid" in {

               Monoid[Int].combine(23, 17) shouldEqual 40
               Monoid[Int].combineN(2, 5) shouldEqual 10
               Monoid[Int].combineAll(Seq(1,2,3,1,8,9, 10)) shouldEqual 34
               Monoid[Int].isEmpty(4) shouldEqual false
               Monoid[Int].empty shouldEqual 0
          }

          "-> String is a Monoid" in {

               Monoid[String].combine("mountain", " cabin") shouldEqual "mountain cabin"
               Monoid[String].combineN("meow-", 3).dropRight(1) shouldEqual "meow-meow-meow"
               Monoid[String].combineAll(Seq("a","l","p","h","a","b","e","t")) shouldEqual "alphabet"
               Monoid[String].empty shouldEqual ""
          }*/

}
