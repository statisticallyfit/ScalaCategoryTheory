package TEMP_LINALG.theory.space

import TEMP_LINALG.theory._

/**
  * A vector space is a set V together with two binary operations that combine
  * two entities to yield a third, called vector addition and scalar multiplication.
  * Vector spaces fall into two categories: A vector space V is said to be finite-dimensional if there is a finite set
  * of vectors in V that spans V and is said to be infinite-dimensional if no such set exists.
  *
  * Laws:
  * (i) a + b = b + a                   --- commutativity, vector addition
  * (ii) (a + b) + c = a + (b + c)      --- associativity, vector addition
  * (iii) k(a + b) = ka + kb            --- distributivity, vector addition
  * (iv) k(cu) = (kc) u                 --- associativity, scalar multiplication
  * (v) k + c)u = ku + cu               --- distributivity, scalars:
  *
  */
trait VectorSpace[V, F] extends AbelianGroup[V] with Monoid[V] {

     //implicit def dimensionOfVectorSpace: Dimension[V]
     //implicit val scalar: Field[F]

     val zero: V
     val one: V

     def plus(v: V, w: V): V //addition => u + v
     def negate(v: V): V //additive inverse => u + (-u) = 0
     def scale(v: V, constant: F): V //scalar multiplication: ku

     //def minus(v: V, w: V): V = plus(v, negate(w))
}

object VectorSpace {
     final def apply[V, R](implicit ev: VectorSpace[V, R]): VectorSpace[V, R] = ev
}
