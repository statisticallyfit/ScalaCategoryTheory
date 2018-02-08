package TEMP_LINALG.theory.space

/**
  * A Hilbert space is an inner product space, an abstract vector space in which distances and angles
  * can be measured. It is also "complete", meaning that if a sequence of vectors is Cauchy, then it
  * converges to some limit in the space.
  *
  */
trait HilbertSpace[H, F] extends InnerProductSpace[H, F] {
     //∠ : H × H → F
     // Inner product formalizes the geometrical notions such as the length of a vector and the angle between two vectors.
     def angle(v: H, w: H): F

     // <⋅,⋅> : H × H → F
     // Inner product formalizes the geometrical notions such as the length of a vector and the angle between two vectors.
     //def dotProduct(that: H): F
}


object HilbertSpace {
     final def apply[H, R](implicit ev: HilbertSpace[H, R]): HilbertSpace[H, R] = ev
}
