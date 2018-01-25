import cats.{Eq, Functor}
import cats.implicits._
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import functor.data.LiftItOut._



case class LiftItOut[A, B](lifter: A => B)

//final case class Lift[+A, +B](lifter: A => B) extends LiftItOut[A, B]
object LiftItOut {

     implicit def liftFunctor[A] = new Functor[LiftItOut[A, ?]]{

          def map[B, C](fa: LiftItOut[A, B])(f: B => C): LiftItOut[A, C] ={

               fa match {
                    case LiftItOut(ab) => LiftItOut(f.compose(ab))
               }
          }
     }

     implicit def liftEq[A, B] = new Eq[LiftItOut[A, B]] {
          def eqv(l1: LiftItOut[A, B], l2: LiftItOut[A, B]): Boolean = true //todo superficial def
     }
}

def lifter(s: String): Int = s.length
val str: String = "dolphins"

LiftItOut(lifter(str))

