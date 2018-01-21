package functor.data


import cats.data.Validated
import cats.data.Validated.Invalid
import cats.{Eq, Functor, Monoid}



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

/*case class Two[A, B](aValue: A, bValue: B)

object Two {
     //type Temp[A] = Two[A, B]
     implicit def functorTwo[ B]: Functor[Two[?,B]] = new Functor[Two[?,B]] {

          def map[A,_](fa: Two[A, B])(f: A => B): Two[A, B] = Two( fa.aValue, f(fa.aValue))
     }
}*/


// ------------------------------------------------------------------------------------------

/*class Sum[B, A]

object Sum {

     case class First[A](first: A) extends Sum[Nothing, A]
     case class Second[B](second: B) extends Sum[B, Nothing]

     implicit def sumFunctor[A] = new Functor[Sum[?, A]] {
          def map[_, B](fa: Sum[B, A])(f: A => B): Sum[B, B] ={
               fa match {
                    case First(a) => First(f(a))
                    case Second(b) => Second(b)
               }
          }
     }
}*/


// ------------------------------------------------------------------------------------------

class Quant[A, B]
object Quant {

     /*
     implicit def catsStdInstancesForEither[A]: MonadError[Either[A, ?], A] with Traverse[Either[A, ?]] =
    new MonadError[Either[A, ?], A] with Traverse[Either[A, ?]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
        fa.right.flatMap(f)
     */
     case class Finance()
     case class Desk[A](desk: A) extends Quant[A, Nothing]
     case class Bloor[B](bloor: B) extends Quant[Nothing, B]

     implicit def quantFunctor[B] = new Functor[Quant[?, B]] {
          def map[A, _](fa: Quant[A, B])(f: A => B) ={
               fa match {
                    case Finance() => Finance()
                    case Desk(a) => Desk(a)
                    case Bloor(b) => Bloor(f(b))
               }
          }
     }
}