package TEMP_LINALG.syntax

import TEMP_LINALG._
/**
  *
  */
object EqualSyntax {
     implicit class EqualityOps[E: Equal](current: E){
          private val eq = implicitly[Equal[E]]

          def :==:(other: E): Boolean = eq.equal(current, other)
          def !==(other: E): Boolean = ! eq.equal(current, other)
          def <(other: E): Boolean = eq.lessThan(current, other)
          def >(other: E): Boolean = eq.greaterThan(current, other)
          def <=(other: E): Boolean = eq.lessThanOrEqual(current, other)
          def >=(other: E): Boolean = eq.greaterThanOrEqual(current, other)
     }
}
