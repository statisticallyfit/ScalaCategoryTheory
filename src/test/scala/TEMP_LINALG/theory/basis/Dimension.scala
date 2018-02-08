package TEMP_LINALG.theory.basis


/**
  *
  */

trait Dimension[V]{

     def dimension(v: V): Int
}

object Dimension {
     final def apply[V](implicit ev: Dimension[V]): Dimension[V] = ev
}
