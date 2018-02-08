package TEMP_LINALG.theory.basis

/**
  *
  * pg 342 for linear transformation LAWS (addition and scalar multiplication)
  *
  */
trait Transformation[V, W] {

     def transform(x: V): W
}



object Transformation {
     final def apply[V, W](implicit ev: Transformation[V, W]): Transformation[V, W] = ev
}