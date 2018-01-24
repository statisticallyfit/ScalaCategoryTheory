import cats.{Eq, Functor}
import cats.implicits._
import cats.instances.AllInstances
import cats.syntax.AllSyntax




sealed abstract class BinaryTree2[+A]
case object Leaf2 extends BinaryTree2[Nothing]
//final case class Leaf2[+A](value: A) extends BinaryTree2[A] //note functor composition not working with this
// definition why?? todoo wfind out why
final case class Branch2[+A](left: BinaryTree2[A], mid: A, right: BinaryTree2[A]) extends BinaryTree2[A]

//note changing this def since functor composition not working with previous def.

object BinaryTree2 {
     implicit def treeFunctor2 = new Functor[BinaryTree2] {

          def map[A, B](fa: BinaryTree2[A])(f: A => B): BinaryTree2[B] ={
               fa match {
                    case Leaf2 => Leaf2
                    case Branch2(left, mid, right) =>
                         Branch2(map(left)(f), f(mid), map(right)(f))
               }
          }
     }

     implicit def treeEq2[A: Eq] = new Eq[BinaryTree2[A]] {

          def eqv(tree1: BinaryTree2[A], tree2: BinaryTree2[A]): Boolean ={
               (tree1, tree2) match {
                    case (Leaf2, Leaf2) => true //Eq[A].eqv(a1, a2)
                    case (Branch2(l1, m1, r1), Branch2(l2, m2, r2)) => eqv(l1, l2) && eqv(r1, r2) && Eq[A].eqv(m1,m2)
                    case _ => false
               }
          }
     }
}

val tree: BinaryTree2[Int] = Branch2(Leaf2, 12, Leaf2)

tree.map(identity) === Branch2(Leaf2, 12, Leaf2)








//---------------------
import org.scalacheck.Prop.forAll
import org.scalacheck._


object tester extends Properties ("tester"){
     property(s"NAME.identity: fa.map(identity) === fa") ={
          forAll {(fa: BinaryTree2[Int]) =>
               fa.map(identity) === fa
          }
     }
}
