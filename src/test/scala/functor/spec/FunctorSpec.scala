package functor.spec


import functor.data._

import cats.Functor
import cats.data.Validated.{Valid, Invalid}
import cats.implicits._

import org.specs2.mutable._
import org.scalacheck.Arbitrary

/**
  *
  */

class FunctorSpec extends Specification {

     "Functor is a typeclass that can be used to map functions through a type" should {

          "-> Option is a functor" in {


          }

          "-> List is a functor" in {

               "---> functions can be mapped. "{
                    val list = List(1,2,3,4,5)

                    Functor[List].map(list)(_ * 12) shouldEqual List(12, 24, 36, 48, 60)

                    list.map(_ * 12) shouldEqual List(12, 24, 36, 48, 60)

                    list.map(_ * 3) shouldEqual List(3, 6, 9, 12, 15)
               }

               "---> functions can be composed. " in {

                    import cats.instances.list._

                    val f = (_: Int) * 3
                    val g = (_: Int) + 1

                    val anyList:List[Int] = Arbitrary.arbitrary[List[Int]].sample.get

                    val firstMapping = anyList.map(g.compose(f))
                    val secondMapping = anyList.map((f).map(g))

                    firstMapping shouldEqual secondMapping
               }
          }
     }
}
