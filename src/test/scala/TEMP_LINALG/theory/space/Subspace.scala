package TEMP_LINALG.theory.space

import TEMP_LINALG._

/**
  * A non-empty subset S of vector space V is a subspace of V if it also
  * satisfies the ten axioms of a vector space.
  */
trait Subspace[S, F] extends VectorSpace[S, F] {

     //return the basis of the subspace W of vecspace V, because that is the generating set of W (basis)
     def subspace(vset: VectorSet[F]): VectorSet[S] //same implementation as span(vset)

     def isSubsetOf(subset: S, parent: S): Boolean// implicit class has: parent.isSubset(subset): Boolean

     def isSubspaceOf(subspace: S, parent: S): Boolean
}


object Subspace {
     final def apply[S, R](implicit ev: Subspace[S, R]): Subspace[S, R] = ev
}
