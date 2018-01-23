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

               ".   -> mapping: we can map a function" in {
                    val list = List(1,2,3,4,5)

                    Functor[List].map(list)(_ * 12) shouldEqual List(12, 24, 36, 48, 60)

                    list.map(_ * 12) shouldEqual List(12, 24, 36, 48, 60)

                    list.map(_ * 3) shouldEqual List(3, 6, 9, 12, 15)

                    List(2,4,9,11).map(x => x % 2 == 0) shouldEqual List(true, true, false, false)
               }

               ".   -> composition: we can compose several functions" in {

                    List(1,2,3).map(_ * 8) shouldEqual List(8, 16, 24)

                    List(1,2,3).map(x => (x * 2) + 4) shouldEqual List(6, 8, 10)

                    Functor[List].map(List(1,2,3))(_ * 2).map(_ + 4).map(_ + 1) shouldEqual List(7,9,11)
                    List(1,2,3).map(_ * 2).map(_ + 4).map(_ + 1) shouldEqual List(7, 9, 11)
               }

               ".   -> lifting: we can apply/lift a function to a value" in {
                    val liftCountLength = Functor[List].lift((s: String) => s.length)

                    liftCountLength(List("merry","king","of","the","bush")) shouldEqual List(5,4,2,3,4)
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {
                    Functor[List].fproduct(List(8,1,2))(_ + 4) shouldEqual List((8,12), (1,5), (2,6))
                    List(8,1,2).fproduct(_ + 4) shouldEqual List((8,12), (1,5), (2,6))
               }

               ".   -> laws" in {

                    val anyList = Arbitrary.arbitrary[List[Int]].sample.get
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {
                         anyList.map(identity) shouldEqual anyList
                         Functor[List].map(anyList)(identity) shouldEqual anyList
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         anyList.map(g compose f) shouldEqual anyList.map(f).map(g)
                         Functor[List].map(anyList)(g compose f) shouldEqual Functor[List].map(anyList)(f).map(g)
                         //Functor[List].map(anyList)(f map g) shouldEqual Functor[List].map(anyList)(f).map(g)
                    }
               }
          }

          // ---------------------------------------------------------------------------

          "-> Option is a functor" in {

               ".   -> mapping: we can map a function" in {

                    Option(1).map(_ * 9) shouldEqual Some(9)
                    Functor[Option].map(Option(1))(_ * 9) shouldEqual Some(9)

                    Some(5).map(_ + 2) shouldEqual Some(7)
                    Functor[Option].map(Some(5))(_ + 3) shouldEqual Some(8)

                    None.map((x: Int) => x + 1) shouldEqual None
                    Functor[Option].map(None)((x: Int) => x + 1) shouldEqual None

                    None.map((x: Int) => x.toString) shouldEqual None
                    Functor[Option].map(None)(_.toString) shouldEqual None
               }

               ".   -> composition: we can compose several functions" in {

                    Some(3).map(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Some(-2)
                    Functor[Option].map(Some(3))(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Some(-2)

                    Some("eight").map(_.length) shouldEqual Some(5)
                    Functor[Option].map(Some("eight"))(_.length) shouldEqual Some(5)

                    None.map((x: String) => x.length) shouldEqual None
                    Functor[Option].map(None)((x: String) => x.length) shouldEqual None
               }

               ".   -> lifting: we can apply/lift a function to a value" in {

                    val liftTimesTwelve = Functor[Option].lift((x: Int) => x * 12)

                    liftTimesTwelve(Some(3)) shouldEqual Some(36)
                    liftTimesTwelve(None) shouldEqual None
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {
                    Functor[Option].fproduct(Some(8))(_ + 9) shouldEqual Some((8, 17))
                    Option(8).fproduct(_ + 9) shouldEqual Some((8, 17))

                    Functor[Option].fproduct(None)((x:Int) => x + 9) shouldEqual None
                    None.fproduct((x:Int) => x + 9) shouldEqual None
               }

               ".   -> laws" in {
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {
                         Some(23).map(identity) shouldEqual Some(23)
                         Functor[Option].map(Some(23))(identity) shouldEqual Some(23)

                         None.map(identity) shouldEqual None
                         Functor[Option].map(None)(identity) shouldEqual None
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         Some(23).map(g compose f) shouldEqual Some(23).map(f).map(g)
                         Functor[Option].map(Some(23))(g compose f) shouldEqual Functor[Option].map(Some(23))(f).map(g)

                         None.map(g compose f) shouldEqual None.map(f).map(g)
                         Functor[Option].map(None)(g compose f) shouldEqual Functor[Option].map(None)(f).map(g)
                         //Functor[List].map(anyList)(f map g) shouldEqual Functor[List].map(anyList)(f).map(g)
                    }
               }
          }
          //either next
          //future
          //then compose types: list with option, tree with option, sum with list, etc ...
          //then some of my types: Tree ,...

          //note: we write only stuff like Functor[Option] compose Functor[List] at the END and not for each
          // note individual type in order to avoid repetition!
     }
}
