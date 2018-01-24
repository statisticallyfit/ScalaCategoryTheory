package functor.spec

import functor.data._
import functor.data.ArbitraryADTs._

import cats.Functor
import cats.Functor._
import cats.implicits._
import cats.instances.AllInstances
//import cats.instances.either._

import org.specs2.mutable.Specification
import org.scalacheck.Arbitrary
/**
  *
  */
class bintreespec extends Specification with AllInstances {

     "-> BinaryTree2 is a functor" should {
          val tree: BinaryTree2[Int] = Branch2(Branch2(Leaf2, 4, Leaf2), 23, Branch2(Branch2(Leaf2, 5, Leaf2), 1, Leaf2))

          ".   -> BINARY TREE_2 laws" in {
               val f = (_:Int) * 3
               val g = (_:Int) + 1

               val anyTree = Arbitrary.arbitrary[BinaryTree2[Int]].sample.get
               println(anyTree)

               ".     -> law 1: identity: mapping the identity function should give the original value" in {

                    tree.map(identity) shouldEqual tree
                    Functor[BinaryTree2].map(tree)(identity) shouldEqual tree

                    anyTree.map(identity) shouldEqual anyTree
                    Functor[BinaryTree2].map(anyTree)(identity) shouldEqual anyTree
               }

               ".     -> law 2: composition: mapping a composed function on a functor is the " +
                    "same as mapping the functions one by one" in {

                    tree.map(g compose f) shouldEqual (tree.map(f).map(g))

                    val composeValue = Functor[BinaryTree2].map(tree)(g compose f)
                    val mapSequentiallyValue = (Functor[BinaryTree2].map(tree)(f)).map(g)

                    composeValue shouldEqual mapSequentiallyValue

                    //---
                    anyTree.map(g compose f) shouldEqual (anyTree.map(f).map(g))
               }
          }
     }

}
