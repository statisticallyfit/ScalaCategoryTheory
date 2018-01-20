package functor.data


import cats.{Eq, Monoid, Functor}



case class Identity[A](value: A)