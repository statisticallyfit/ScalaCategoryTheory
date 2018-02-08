package TEMP_LINALG.syntax

import TEMP_LINALG._
/**
  *
  */
object TrigSyntax {
     implicit class TrigOps[T: Trig](current: T) {
          private val trig = implicitly[Trig[T]]

          /*val E: T = trig.E // note - I don't want objects to access these with '.' operator, nor theta.
          val PI: T = trig.PI*/

          def sin(): T = trig.sin(current)
          def cos(): T = trig.cos(current)
          def tan(): T = trig.tan(current)
          def csc(): T = trig.csc(current)
          def sec(): T = trig.sec(current)
          def cot(): T = trig.cot(current)

          def arcsin(): T = trig.arcsin(current)
          def arccos(): T = trig.arccos(current)
          def arctan(): T = trig.arctan(current)
          def arccsc(): T = trig.arccsc(current)
          def arcsec(): T = trig.arcsec(current)
          def arccot(): T = trig.arccot(current)
     }
}
