package TEMP_LINALG.theory.space

import TEMP_LINALG.theory._
import TEMP_LINALG._


/**
  *
  */

trait NormedVectorSpace[V, F] extends VectorSpace[V, F] {

     //this: Field[F] =>

     //note defining norm() just in normedinnerprodspace only - normedvecspace doesn't know about innerprod.
     def norm(v: V): F
     def normalize(v: V): V ={
          implicit val scalar: Field[F] = norm(v).asInstanceOf[Field[F]] //todo major fix!
          scale(v, scalar.inverse(norm(v)))
     }
     def isNormalized(v: V)(implicit eq: Equal[V]): Boolean = eq.equal(v, normalize(v))
     def distance(v: V, w: V): F = norm(plus(v, negate(w)))
}


object NormedVectorSpace extends NormedVectorSpaceBase  {
     final def apply[V, R](implicit ev: NormedVectorSpace[V, R]): NormedVectorSpace[V, R] = ev
}

private[space] trait NormedVectorSpaceBase {
     implicit def InnerProductSpaceIsNormedVectorSpace[V, F](implicit space: InnerProductSpace[V, F], nroot:
     Root[F, F]): NormedVectorSpace[V, F] = space.normed
}