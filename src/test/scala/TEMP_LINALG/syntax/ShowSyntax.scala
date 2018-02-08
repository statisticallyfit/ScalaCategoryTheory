package TEMP_LINALG.syntax

import TEMP_LINALG._
/**
  *
  */
object ShowSyntax {
     implicit class ShowOps[S: Show](current: S) {
          val ev = implicitly[Show[S]]

          def show: String = ev.show(current)
     }
}
