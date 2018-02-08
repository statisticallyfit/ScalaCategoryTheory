package TEMP_LINALG.theory.basis

import TEMP_LINALG.theory.space._

/**
  *
  */

trait OrthogonalSpace[V, F] extends VectorSpace[V, F] {

     def isOrthogonal(v: V): Boolean
     def areOrthogonal(v1: V, v2: V): Boolean
     def orthogonalize(v: V): V
}



object OrthogonalSpace {
     final def apply[V, R](implicit ev: OrthogonalSpace[V, R]): OrthogonalSpace[V, R] = ev
}