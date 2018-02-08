package TEMP_LINALG.theory.basis

import TEMP_LINALG.theory.space._

/**
  *
  */
trait Basis[B, F] extends VectorSpace[B, F] with Span[B, F] {

     //note ifvecset cols are linearly independent, then the vecset is a basis for vecpsace V^n,
     // if not return None. which means this vecset is not a basis for the V^n vecspace.
     // prereq is isBasisOfSpaceWith function
     def basis(vspace: B): Option[B]

     // is basis: B a basis for the vset?
     def isBasis(vset: B, basis: B): Boolean // like isSpanned(vset, v)
     //is v in basis of vset?
     def isInBasis(v: B, vset: B): Boolean = isBasis(vset, v) //todo is this correct?

     // is vset a basis for vecspace B?
     def isBasis(vset: B): Boolean // like isSpanned(vset)
}


object Basis {
     final def apply[V, R](implicit ev: Basis[V, R]): Basis[V, R] = ev
}