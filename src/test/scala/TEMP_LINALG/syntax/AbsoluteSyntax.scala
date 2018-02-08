package TEMP_LINALG.syntax

import TEMP_LINALG._
/**
  *
  */
object AbsoluteSyntax {
     implicit class AbsoluteOps[H, L](current: H)(implicit pos: Absolute[H, L]){
          def abs(): L = pos.absoluteValue(current)
     }
}
