package functor.spec


import functor.data._
import functor.data.ArbitraryADTs._

import cats.Functor
import cats.implicits._
import cats.instances.AllInstances
import cats.syntax.AllSyntax

import org.specs2.mutable._
import org.scalacheck.Arbitrary

/**
  *
  */

class FunctorSpec extends Specification with AllInstances with AllSyntax {

     "Functor is a typeclass that can be used to map functions through a type" should {

          "-> List[A] is a functor" in  {

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

          "-> Option[A] is a functor" in {

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
                    }
               }
          }

          // ---------------------------------------------------------------------------

          "-> Either[E,A] is a functor" in {

               //import cats.instances.either._

               ".   -> mapping: we can map a function" in {

                    Right(6).map(_ * 3) shouldEqual Right(18)
                    Functor[Either[String, ?]].map(Right(6))(_ * 3) shouldEqual Right(18)

                    Left("hi").map((s: String) => s + " there") shouldEqual Left("hi")
                    Functor[Either[String, ?]].map(Left("hi"))((s:String) => s.length) shouldEqual Left("hi")
               }

               ".   -> composition: we can compose several functions" in {

                    Right(30).map(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Right(52)
                    Functor[Either[String, ?]].map(Right(30))(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Right(52)

                    Left("ice").map((s: String) => s + " cream") shouldEqual Left("ice")
                    Functor[Either[String, ?]].map(Left("ice"))((s: String) => s + " cream") shouldEqual Left("ice")
               }

               ".   -> lifting: we can apply/lift a function to a value" in {

                    val liftTimesTwelve = Functor[Either[String, ?]].lift((x: Int) => x * 12)

                    liftTimesTwelve(Right(45)) shouldEqual Right(540)
                    liftTimesTwelve(Left("blahblahblah")) shouldEqual Left("blahblahblah")
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {

                    //todo Right(2).fproduct(_ + 7) shouldEqual Right((2, 14))
                    Functor[Either[String, ?]].fproduct(Right(2))(_ + 7) shouldEqual Right((2, 9))

                    //todo Left("eeck").fproduct((s:String) => s.length) shouldEqual Left("eeck")
                    Functor[Either[String, ?]].fproduct(Left("eeck"))((s:String) => s.length) shouldEqual Left("eeck")
               }

               ".   -> laws" in {
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {
                         Right(23).map(identity) shouldEqual Right(23)
                         Functor[Either[String, ?]].map(Right(23))(identity) shouldEqual Right(23)

                         Left("nothing").map(identity) shouldEqual Left("nothing")
                         Functor[Either[String, ?]].map(Left("nothing"))(identity) shouldEqual Left("nothing")
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         Right(23).map(g compose f) shouldEqual Right(23).map(f).map(g)

                         val composeValueRight = Functor[Either[String, ?]].map(Right(23))(g compose f)
                         val mapSequentiallyValueRight = Functor[Either[String, ?]].map(Right(23))(f).map(g)
                         composeValueRight shouldEqual mapSequentiallyValueRight

                         //---
                         Left("nothing").map(g compose f) shouldEqual Left("nothing").map(f).map(g)

                         val composeValueLeft = Functor[Either[String, ?]].map(Left("nothing"))(g compose f)
                         val mapSequentiallyValueLeft = Functor[Either[String, ?]].map(Left("nothing"))(f).map(g)
                         composeValueLeft shouldEqual mapSequentiallyValueLeft
                    }
               }
          }

          // ---------------------------------------------------------------------------

          "-> Pair[A, A] is a functor" in {

               import functor.data.Pair._

               ".   -> mapping: we can map a function" in {

                    Pair(1, 2).map(_ + 4) shouldEqual Pair(5, 6)
                    Functor[Pair].map(Pair(1,2))(_ + 4) shouldEqual Pair(5, 6)
               }

               ".   -> composition: we can compose several functions" in {

                    Pair(3, 7).map(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Pair(-2, 6)
                    Functor[Pair].map(Pair(3, 7))(_ + 1).map(_ - 5).map(_ * 2) shouldEqual Pair(-2, 6)
               }

               ".   -> lifting: we can apply/lift a function to a value" in {

                    val liftTimesTwelve = Functor[Pair].lift((x: Int) => x * 12)

                    liftTimesTwelve(Pair(1, 3)) shouldEqual Pair(12, 36)
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {

                    Pair(11, -8).fproduct(_ + 7) shouldEqual Pair((11, 18), (-8, -1))
                    Functor[Pair].fproduct(Pair(11, -8))(_ + 7) shouldEqual Pair((11, 18), (-8, -1))
               }

               ".   -> laws" in {
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {
                         Pair(23, 1).map(identity) shouldEqual Pair(23, 1)
                         Functor[Pair].map(Pair(23, 1))(identity) shouldEqual Pair(23, 1)
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         Pair(23, 1).map(g compose f) shouldEqual Pair(23, 1).map(f).map(g)

                         val composeValue = Functor[Pair].map(Pair(23, 1))(g compose f)
                         val mapSequentiallyValue = Functor[Pair].map(Pair(23, 1))(f).map(g)
                         composeValue shouldEqual mapSequentiallyValue
                    }
               }
          }

          // ---------------------------------------------------------------------------

          "-> Three[A,B,C] is a functor" in {

               import functor.data.Three._

               ".   -> mapping: we can map a function" in {

                    val triple: Three[String,Int,Option[Int]] = Three("word", 2, Some(4))
                    val result: Three[String,Int,Option[Int]] = Three("word", 2, Some(5))

                    //todo triple.map(_.map(_ + 1)) shouldEqual result
                    Functor[Three[String, Int, ?]].map(triple)(_.map(_ + 1)) shouldEqual result
               }

               ".   -> composition: we can compose several functions" in {

                    val triple: Three[Int,Int,Int] = Three(1,1, 2)
                    val result: Three[Int,Int,Int] = Three(1,1, -4)

                    //todo ((triple.map(_ + 1)).map(_ - 5)).map(_ * 2) shouldEqual result
                    ((Functor[Three[Int,Int,?]].map(triple)((x:Int) => x + 1)).map((x:Int) => x - 5)).map((x:Int) => x * 2) shouldEqual result
               }

               ".   -> lifting: we can apply/lift a function to a value" in {

                    val triple: Three[Int,Int,Int] = Three(1,1, 2)
                    val result: Three[Int,Int,Int] = Three(1,1, 24)
                    val liftTimesTwelve = Functor[Three[Int, Int, ?]].lift((x: Int) => x * 12)

                    liftTimesTwelve(triple) shouldEqual result
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {

                    val triple: Three[Int,Int,Int] = Three(1,1, 2)
                    val result: Three[Int,Int,(Int,Int)] = Three(1,1, (2, 9))

                    //todo triple.fproduct(_ + 7) shouldEqual result
                    Functor[Three[Int, Int, ?]].fproduct(triple)(_ + 7) shouldEqual result
               }

               ".   -> laws" in {
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    val triple: Three[Int,Int,Int] = Three(1,1, 2)

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {

                         //todo triple.map(identity) shouldEqual triple
                         Functor[Three[Int, Int, ?]].map(triple)(identity) shouldEqual triple
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         //todo triple.map(g compose f) shouldEqual triple.map(f).map(g)

                         val composeValue = Functor[Three[Int, Int, ?]].map(triple)(g compose f)
                         val mapSequentiallyValue = Functor[Three[Int, Int, ?]].map(triple)(f).map(g)

                         composeValue shouldEqual mapSequentiallyValue
                    }
               }
          }

          // ---------------------------------------------------------------------------

          "-> BinaryTree[T] is a functor" in {

               import functor.data.BinaryTree._

               val tree: BinaryTree[Int] = Branch(Branch(Leaf(1), 4, Leaf(5)), 23,
                    Branch(Branch(Leaf(2), 5, Leaf(2)), 1, Leaf(9)))

               ".   -> mapping: we can map a function" in {

                    val result: BinaryTree[Int] = Branch(Branch(Leaf(2), 8, Leaf(10)), 46, Branch(Branch(Leaf(4),10,
                         Leaf(4)), 2, Leaf(18)))

                    tree.map(_ * 2) shouldEqual result
                    Functor[BinaryTree].map(tree)(_ * 2) shouldEqual result
               }

               ".   -> composition: we can compose several functions" in {

                    val result: BinaryTree[Int] = Branch(Branch(Leaf(-6), 0, Leaf(2)), 38,
                         Branch(Branch(Leaf(-4), 2, Leaf(-4)), -6, Leaf(10)))

                    tree.map(_ + 1).map(_ - 5).map(_ * 2) shouldEqual result
                    Functor[BinaryTree].map(tree)(_ + 1).map(_ - 5).map(_ * 2) shouldEqual result
               }

               ".   -> lifting: we can apply/lift a function to a value" in {

                    val liftTimesTwelve = Functor[BinaryTree].lift((x: Int) => x * 12)

                    val result: BinaryTree[Int] = Branch(Branch(Leaf(12), 48, Leaf(60)), 276,
                         Branch(Branch(Leaf(24), 60, Leaf(24)), 12, Leaf(108)))

                    liftTimesTwelve(tree) shouldEqual result
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {

                    val result: BinaryTree[(Int,Int)] = Branch(Branch(Leaf((1,8)), (4,11), Leaf((5,12))), (23,30),
                         Branch(Branch(Leaf((2,9)), (5,12), Leaf((2,9))), (1,8), Leaf((9,16))))

                    tree.fproduct(_ + 7) shouldEqual result
                    Functor[BinaryTree].fproduct(tree)(_ + 7) shouldEqual result
               }

               ".   -> laws" in {
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    val anyTree = Arbitrary.arbitrary[BinaryTree[Int]].sample.get

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {

                         tree.map(identity) shouldEqual tree
                         Functor[BinaryTree].map(tree)(identity) shouldEqual tree

                         anyTree.map(identity) shouldEqual anyTree
                         Functor[BinaryTree].map(anyTree)(identity) shouldEqual anyTree
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         tree.map(g compose f) shouldEqual (tree.map(f).map(g))

                         val composeValue = Functor[BinaryTree].map(tree)(g compose f)
                         val mapSequentiallyValue = (Functor[BinaryTree].map(tree)(f)).map(g)

                         composeValue shouldEqual mapSequentiallyValue

                         //---
                         anyTree.map(g compose f) shouldEqual (anyTree.map(f).map(g))
                    }
               }
          }

          // ---------------------------------------------------------------------------

          "-> LiftItOut[A, B] is a functor" in {

               import functor.data.LiftItOut._

               def lft(a: String): Int = a.length
               def timesTwo(x: Int): Int = x * 2
               val str: String = "dolphin"

               ".   -> mapping: we can map a function" in {

                    //todo LiftItOut(lft).map((x:Int) => x * 2).lifter(str) shouldEqual 14
                    Functor[LiftItOut[String, ?]].map(lft)((x:Int) => x * 2).lifter(str) shouldEqual 14
               }

               /*".   -> composition: we can compose several functions" in {

                    LiftItOut(lft).map(_ + 1).map(_ - 5).map(_ * 2).lifter(str) shouldEqual result
                    Functor[BinaryTree].map(tree)(_ + 1).map(_ - 5).map(_ * 2) shouldEqual result
               }

               ".   -> lifting: we can apply/lift a function to a value" in {

                    val liftTimesTwelve = Functor[BinaryTree].lift((x: Int) => x * 12)

                    val result: BinaryTree[Int] = Branch(Branch(Leaf(12), 48, Leaf(60)), 276,
                         Branch(Branch(Leaf(24), 60, Leaf(24)), 12, Leaf(108)))

                    liftTimesTwelve(tree) shouldEqual result
               }

               ".   -> fproduct: pairs source value with result of applying a function" in {

                    val result: BinaryTree[(Int,Int)] = Branch(Branch(Leaf((1,8)), (4,11), Leaf((5,12))), (23,30),
                         Branch(Branch(Leaf((2,9)), (5,12), Leaf((2,9))), (1,8), Leaf((9,16))))

                    tree.fproduct(_ + 7) shouldEqual result
                    Functor[BinaryTree].fproduct(tree)(_ + 7) shouldEqual result
               }

               ".   -> laws" in {
                    val f = (_:Int) * 3
                    val g = (_:Int) + 1

                    val anyTree = Arbitrary.arbitrary[BinaryTree[Int]].sample.get

                    ".     -> law 1: identity: mapping the identity function should give the original value" in {

                         tree.map(identity) shouldEqual tree
                         Functor[BinaryTree].map(tree)(identity) shouldEqual tree

                         anyTree.map(identity) shouldEqual anyTree
                         Functor[BinaryTree].map(anyTree)(identity) shouldEqual anyTree
                    }

                    ".     -> law 2: composition: mapping a composed function on a functor is the " +
                         "same as mapping the functions one by one" in {

                         tree.map(g compose f) shouldEqual (tree.map(f).map(g))

                         val composeValue = Functor[BinaryTree].map(tree)(g compose f)
                         val mapSequentiallyValue = (Functor[BinaryTree].map(tree)(f)).map(g)

                         composeValue shouldEqual mapSequentiallyValue

                         //---
                         anyTree.map(g compose f) shouldEqual (anyTree.map(f).map(g))
                    }
               }*/
          }
          //future
          //then compose types: list with option, tree with option, sum with list, etc ...
          //then some of my types: Tree ,...

          //note: we write only stuff like Functor[Option] compose Functor[List] at the END and not for each
          // note individual type in order to avoid repetition!
     }
}
