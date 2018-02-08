package TEMP_LINALG.syntax

import TEMP_LINALG._
import scala.language.higherKinds
/**
  *
  */
object VectorLikeSyntax {

     /*implicit class VectorOps[N: Number: Trig](current: Vector[N])(implicit root: Root[N,N],
                                                                   vectorLike: VectorLike[Vector[N], N]){

          def add(other:  Vector[N]):  Vector[N] = vectorLike.plus(current, other)
          def -(other:  Vector[N]):  Vector[N] = vectorLike.minus(current, other)
          def negate():  Vector[N] = vectorLike.negate(current)
          def scale(factor: N):  Vector[N] = vectorLike.scale(current, factor)

          def norm(): N = vectorLike.norm(current)
          def angle(other:  Vector[N]): N = vectorLike.angle(current, other)
          def innerProduct(other:  Vector[N]): N = vectorLike.innerProduct(current, other)
          def dotProduct(other:  Vector[N]): N = vectorLike.dotProduct(current, other)
          def crossProduct(other:  Vector[N]):  Vector[N] = vectorLike.crossProduct(current, other)
     }*/

     implicit class VectorLikeOps[V[_], N: Number: Trig](current: V[N])(implicit root: Root[N,N],
                                                                  vectorLike: VectorLike[V[N], N]){
     /*implicit class VectorLikeOps[V, N: Number: Trig](private val current: V)(implicit root: Root[N,N],
                                                                        vectorLike: VectorLike[V, N]){*/

          def +(other: V[N]): V[N] = vectorLike.plus(current, other)
          def -(other: V[N]): V[N] = vectorLike.minus(current, other)
          def negate(): V[N] = vectorLike.negate(current)
          def scale(factor: N): V[N] = vectorLike.scale(current, factor)

          def norm(): N = vectorLike.norm(current)
          def angle(other: V[N]): N = vectorLike.angle(current, other)
          def innerProduct(other: V[N]): N = vectorLike.innerProduct(current, other)
          def dotProduct(other: V[N]): N = vectorLike.dotProduct(current, other)
          def crossProduct(other: V[N]): V[N] = vectorLike.crossProduct(current, other)

          def isZero(): Boolean = vectorLike.isZero(current)
          /*def +(other: V): V = vectorLike.plus(current, other)
          def add(other: V): V = vectorLike.plus(current, other)
          def -(other: V): V = vectorLike.minus(current, other)
          def negate(): V = vectorLike.negate(current)
          def scale(factor: N): V = vectorLike.scale(current, factor)

          def norm(): N = vectorLike.norm(current)
          def angle(other: V): N = vectorLike.angle(current, other)
          def innerProduct(other: V): N = vectorLike.innerProduct(current, other)
          def dotProduct(other: V): N = vectorLike.dotProduct(current, other)
          def crossProduct(other: V): V = vectorLike.crossProduct(current, other)*/

     }
     /*val v1: Vector[Int] = Vector(1,2,3)
     val v2: Vector[Int] = Vector(1,2,3)
     v1 + v2
     Vector(Real(1), Real(2)) + Vector(Real(1), Real(2))*/
}
