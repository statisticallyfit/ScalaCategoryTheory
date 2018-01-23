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

          "-> List is a functor" in  {

               "-----> mapping" in {
                    val list = List(1,2,3,4,5)

                    Functor[List].map(list)(_ * 12) shouldEqual List(12, 24, 36, 48, 60)

                    list.map(_ * 12) shouldEqual List(12, 24, 36, 48, 60)

                    list.map(_ * 3) shouldEqual List(3, 6, 9, 12, 15)

                    List(2,4,9,11).map(x => x % 2 == 0) shouldEqual List(true, true, false, false)
               }

               "-----> composition" in {

                    List(1,2,3).map(_ * 8) shouldEqual List(8, 16, 24)

                    List(1,2,3).map(x => (x * 2) + 4) shouldEqual List(6, 8, 10)

                    Functor[List].map(List(1,2,3))(_ * 2).map(_ + 4).map(_ + 1) shouldEqual List(7,9,11)
                    List(1,2,3).map(_ * 2).map(_ + 4).map(_ + 1) shouldEqual List(7, 9, 11)


                    // -- more general case
                    val f = (_: Int) * 3
                    val g = (_: Int) + 1

                    val anyList:List[Int] = Arbitrary.arbitrary[List[Int]].sample.get

                    val firstMapping = anyList.map(g.compose(f))
                    val secondMapping = anyList.map(f).map(g)

                    firstMapping shouldEqual secondMapping
               }

               "-----> lifting" in {
                    val liftCountLength = Functor[List].lift((s: String) => s.length)

                    liftCountLength(List("merry","king","of","the","bush")) shouldEqual List(5,4,2,3,4)
               }
          }
          
          // ---------------------------------------------------------------------------

          "-> Option is a functor" in {

               "-----> mapping" in {

                    Option(1).map(_ * 9) shouldEqual Some(9)
                    Functor[Option].map(Option(1))(_ * 9) shouldEqual Some(9)

                    Some(5).map(_ + 2) shouldEqual Some(7)
                    Functor[Option].map(Some(5))(_ + 3) shouldEqual Some(8)

                    None.map((x: Int) => x + 1) shouldEqual None
                    Functor[Option].map(None)((x: Int) => x + 1) shouldEqual None

                    None.map((x: Int) => x.toString) shouldEqual None
                    Functor[Option].map(None)(_.toString) shouldEqual None
               }

               "-----> composition" in {

                    Some(3).map(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Some(-2)
                    Functor[Option].map(Some(3))(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Some(-2)

                    Some("eight").map(_.length) shouldEqual Some(5)
                    Functor[Option].map(Some("eight"))(_.length) shouldEqual Some(5)

                    None.map((x: String) => x.length) shouldEqual None
                    Functor[Option].map(None)((x: String) => x.length) shouldEqual None
               }

               "-----> lifting" in {

                    val liftTimesTwelve = Functor[Option].lift((x: Int) => x * 12)

                    liftTimesTwelve(Some(3)) shouldEqual Some(36)
                    liftTimesTwelve(None) shouldEqual None
               }

          }
          //either next
          //future
          //then compose types: list with option, tree with option, sum with list, etc ...
          //then some of my types: Tree ,...
     }
}
