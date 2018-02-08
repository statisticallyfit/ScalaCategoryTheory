package TEMP_LINALG

/**
  *
  */

import theory.space._
import syntax.TrigSyntax._

import scala.language.implicitConversions
import scala.language.higherKinds


/**
  * Features:
  *
  * //note:
  * - if there is a has-a relation, we use typeclass implementation.
  * If there is is-a relation use trait extension. Example: Vector class HAS-A Basis so declare the
  * Basis[Vector[N] ] implementation bu Vectr IS-A Hilbert Space so extend the Hilbert Space in VectorLike trait.
  *
  * -
  */


trait VectorLike[V, F] extends InnerProductSpace[V, F] with HilbertSpace[V, F] with NormedVectorSpace[V, F] {

     // inherited - plus, negate, scale, innerProduct
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V
}



object VectorLike {

     implicit def VectorIsVectorLike[N: Number: Trig](implicit root: Root[N,N]) = new VectorLike[Vec[N], N] {

          val zero: Vec[N] = Vec(Number.ZERO[N]) //just vector with one element
          val one: Vec[N] = Vec(Number.ONE[N]) //just vector with one element
          //implicit override val scalar: Field[N]


          import syntax.NumberSyntax._

          def plus(v: Vec[N], w: Vec[N]): Vec[N] =
               Vec(v.elems.zip(w.elems).map(pair => pair._1 + pair._2):_*)

          def negate(v: Vec[N]): Vec[N] = Vec(v.elems.map(e => e.negate()):_*)

          def scale(v: Vec[N], factor: N): Vec[N] = Vec(v.elems.map(e => e * factor):_*)

          def isZero(v: Vec[N]): Boolean = v.elems.forall(e => e == Number.ZERO[N])

          def innerProduct(v: Vec[N], w: Vec[N]): N =
               v.elems.zip(w.elems).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)

          def outerProduct(v: Vec[N], w: Vec[N]): Vec[N] = ??? //todo

          def crossProduct(v: Vec[N], w: Vec[N]): Vec[N] = ??? //todo

          def angle(v: Vec[N], w: Vec[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vec[N]): N = v.elems.map(e => root.power(e, Number.TWO[N])).reduceLeft(_ + _)
     }
}


//
// ------------------------------------------------------------------------------------------------------------------------

class Vec[N: Number](val elems: N*){

     import syntax.ShowSyntax._
     override def toString: String = Vec(elems:_*).show
}


object Vec {

     def ZERO[N: Number](len: Int): Vec[N] = Vec(List.fill[N](len)(Number.ZERO[N]):_*)
     def ONE[N: Number](len: Int): Vec[N] = Vec(List.fill[N](len)(Number.ONE[N]):_*)

     def apply[N: Number](elems: N*): Vec[N] = new Vec(elems:_*)
}


// ------------------------------------------------------------------------------------------------------------------------

class VectorSet[N: Number](val cols: Vec[N]*)


object VectorSet {

     //typeclasses ... etc

     implicit class VectorSetOps[V[_], N: Number](vset: VectorSet[N]){
          def reducedRowEchelonForm(): VectorSet[N] = ???
     }
}


// ------------------------------------------------------------------------------------------------------------------------

object VectorTester extends App {

     import syntax.VectorLikeSyntax._
     import VectorLike._


     val v1: Vec[Int] = Vec(1,2,3)
     val v2: Vec[Int] = Vec(2,0,4)
     val v3: Vec[Int] = v1.add(v1)
     v1 + v2

     //     v1 ~ v2
     //     v1.isZero()


     //println(Vector(1,2).innerProduct(v2))
     //println(vec.plus(v1, v2))
     //println(v3)

     //-------
     //note: this did not work either.
     trait Tester[W] {
          def adder(x: W, y: W): String
     }

     implicit class testerops[W, E: RealLike](currentComplex: W)(implicit w: Tester[W]){
          //def testing(evalue: E): String = w.testingMethod(currentComplex, evalue)
          def adder(other: W): String = w.adder(currentComplex, other)
     }

     implicit def ComplexIsTester[R: RealLike](implicit root: Root[R,R]): Tester[Complex[R]] = new Tester[Complex[R]] {
          //def testingMethod(x: Complex[R], real: R): String = s"$x with real $real"
          def adder(current: Complex[R], other: Complex[R]): String = s"$current + $other plussed now"
     }

     val comp: Complex[Int] = new Complex(3, 2)
     comp.adder(comp)
}
