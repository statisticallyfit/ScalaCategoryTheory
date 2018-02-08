package TEMP_LINALG.theory.basis

import TEMP_LINALG.theory.space._
import TEMP_LINALG._

/**
  *
  */
//todo theorem 4.2.6 in howard: implement Eq for Span type


trait Span[V, F] extends VectorSpace[V, F] {

     //gets a set that spans/generates the generic vecspace (gets the basis) todo
     def span(vset: VectorSet[F]): VectorSet[F] // = vset.reducedRowEchelonForm()

     //does vecset span/generate the generic space V (which is like R3)? (example 3.10 singh) - is genspace V spanned
     // by vset?
     //page 198 howard
     //LinearAlgebraToolkit: "Determining if the set spans the space"
     //def isSpanned(vset: VectorSet[F]): Boolean //= vset.reducedRowEchelonForm() === VectorSet.IDENTITY(vset)
     def isSpanned(vset: VectorSet[F]): Boolean

     //does vecset span a specific vector? (example 3.11 singh) Is single vec v spanned by vset? is it in span of
     // vset
     //page 197 howard
     //def isSpanned(vset: VectorSet[F], space: V): Boolean
     //todo implicit conversion from Set[V] to VectorSet[V]
     def isSpanned(vset: VectorSet[F], v: V): Boolean
     def isInSpanOf(v: V, vset: VectorSet[F]): Boolean = isSpanned(vset, v)

     //gets the coefficients the relate the vset to the vector v in the linear combination. They are:
     // k1v1 + k2v2 + k3v3 + ... = v, where vset = {v1,v2,v3...} and v = v.
     def getSpanningCoefficients(vset: VectorSet[F], v: V): Option[VectorSet[F]] //if isspanned is false, then NOne
     // else Some(...)
}


object Span {
     final def apply[S, R](implicit ev: Span[S, R]): Span[S, R] = ev
}