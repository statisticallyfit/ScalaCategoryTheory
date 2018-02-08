package TEMP_LINALG.theory.basis

import TEMP_LINALG.theory.space._

/**
  *
  */

trait OrthonormalSpace[V, F] extends OrthogonalSpace[V, F] with NormedVectorSpace[V, F]  {

     def orthonormalize(v: V): V = normalize( orthogonalize(v) )
}

object OrthonormalSpace {
     final def apply[V, R](implicit ev: OrthonormalSpace[V, R]): OrthonormalSpace[V, R] = ev
}