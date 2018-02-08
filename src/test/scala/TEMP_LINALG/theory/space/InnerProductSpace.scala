package TEMP_LINALG.theory.space

import TEMP_LINALG._





//TODO tomorrow look at spire's methods: https://github.com/non/spire/blob/f86dfda4fb3029f23c023c940ea61dde51e4a0f1/core/shared/src/main/scala/spire/algebra/InnerProductSpace.scala
//TODO next: make the testing things in Discipline (grouplaws, innerprodspace laws, vecspace laws ...etc)
/**
  * An inner product on a real vector space V is an operation <,> which assigns
  * a unique real number to each pair of vectors, u, and v, which satisfies the
  * following axioms for all vectors u,v,w in V and all scalars k.
  *
  * Laws:
  * (i) < u, v> = < v, u >                   --- commutative law
  * (ii) < u+v, w > = < u,w > + < v,w >      --- distributive law
  * (iii) < ku, v > = k< u,v >               --- taking out scalar k
  * (iv) < u,u > >= 0 and we have < u,u> = 0 if and only if u = 0
  *                                          --- (means the inner product is zero or positive)
  *
  */
trait InnerProductSpace[I, F] extends VectorSpace[I, F] { self =>

     def innerProduct(i1: I, i2: I): F
     def dotProduct(i1: I, i2: I): F = innerProduct(i1, i2)

     def normed(implicit ev: Root[F, F]): NormedVectorSpace[I, F] = new NormedInnerProductSpace[I, F] {
          //val dimensionOfVectorSpace = self.dimensionOfVectorSpace
          //this: Field[F] =>

          val root: Root[F, F] = ev
          val innerSpace = self
     }
}

object InnerProductSpace {
     //todo meaning of final?
     final def apply[I, R](implicit ev: InnerProductSpace[I, R]): InnerProductSpace[I, R] = ev
}


// ---------------------------------------------------------------------------------------------------------

//note: I think we have innerprodspace val here to make it specific, not just be a general vecspace.

private[theory] trait NormedInnerProductSpace[V, F] extends NormedVectorSpace[V, F] {


     //this: Field[F] =>

     //val scalar: Field[F] = innerSpace.scalar
     val root: Root[F,F]
     val innerSpace: InnerProductSpace[V, F]

     val zero: V = innerSpace.zero
     val one: V = innerSpace.one
     def plus(v: V, w: V): V = innerSpace.plus(v, w)
     def negate(v: V): V = innerSpace.negate(v)
     def minus(v: V, w: V): V = innerSpace.plus(v, innerSpace.negate(w))
     def scale(v: V, constant: F): V = innerSpace.scale(v, constant)

     def norm(v: V): F = root.squareRoot(innerSpace.innerProduct(v, v))
}