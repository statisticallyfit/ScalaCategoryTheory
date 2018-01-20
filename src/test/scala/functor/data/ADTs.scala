package functor.data


import cats.{Eq, Monoid, Functor}



case class Identity[A](value: A)

object Identity {

     implicit def functorIdentity/*[A, B]*/ = new Functor[Identity] {

          def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity( f(fa.value) )
     }

     implicit def eqIdentity[A : Eq] = new Eq[Identity[A]] {

          def eqv(id1: Identity[A], id2: Identity[A]): Boolean = Eq[A].eqv(id1.value, id2.value)
     }
}


// ------------------------------------------------------------------------------------------

case class Pair[A](aFirst: A, aSecond: A)

object Pair {

     implicit def functorPair: Functor[Pair] = new Functor[Pair]{

          def map[A, B](fa: Pair[A])(f: A => B): Pair[B] = Pair( f(fa.aFirst), f(fa.aSecond) )
     }

     implicit def eqPair[A: Eq] = new Eq[Pair[A]] {

          def eqv(pair1: Pair[A], pair2: Pair[A]): Boolean =
               Eq[A].eqv(pair1.aFirst, pair2.aFirst) && Eq[A].eqv(pair1.aSecond, pair2.aSecond)
     }
}

// ------------------------------------------------------------------------------------------

case class Two[A, B](aValue: A, bValue: B)

object Two {

     implicit def functorTwo[A] = new Functor[Two[A, ?]] {

          def map[C, B](fab: Two[A, B])(f: B => C): Two[A, C] = Two( fab.aValue, f(fab.bValue) )
     }
}


// ------------------------------------------------------------------------------------------