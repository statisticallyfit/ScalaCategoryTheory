package TEMP_LINALG.syntax

import TEMP_LINALG._
/**
  *
  */
object RootSyntax {
     implicit class RootOps[H, L](base: H)(implicit root: Root[H, L]){

          def ^(exp: L): H = root.power(base, exp)
          def sqrt(): H = root.squareRoot(base)
          def nRoot(n: L): H = root.nRoot(base, n)
     }
}
