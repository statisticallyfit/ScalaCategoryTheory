package TEMP_LINALG.theory.space

/**
  * A Banach space, B, is a complete normed vector space such that every Cauchy sequence (with respect
  * to the metric d(x, y) = |x - y|) in B has a limit in B.
  *
  * NOTE: from suanshu
  *
  */

trait BanachSpace[B, F] extends NormedVectorSpace[B, F] {

     // |⋅| : B → F
     //norm assigns a strictly positive length or size to all vectors in the vector space, other than the zero vector.
     /*def norm(v: B): F //note calculates the 2-norm
     def normalize(v: B): B
     def isNormalized(v: B): Boolean*/
}

object BanachSpace {
     final def apply[B, R](implicit ev: BanachSpace[B, R]): BanachSpace[B, R] = ev
}