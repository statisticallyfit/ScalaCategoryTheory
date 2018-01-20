package functor.data


import cats.{Eq, Monoid, Functor}



case class Identity[A](value: A)

object Identity {

     implicit def functorIdentity[A] = new Functor[Identity[A]] {

          def map[_, B](fa: Identity[A])(f: A => B): Identity[B] = {
               Identity(f(fa.value))
          }
     }

     implicit def eqIdentity[A : Eq] = new Eq[Identity[A]] {

          def eqv(id1: Identity[A], id2: Identity[A]): Boolean = Eq[A].eqv(id1.value, id2.value)
     }
}


// ------------------------------------------------------------------------------------------

case class Temp(int: Int)

// ------------------------------------------------------------------------------------------



// ------------------------------------------------------------------------------------------