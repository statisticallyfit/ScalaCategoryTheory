import cats.Functor
import cats.implicits._

Functor[Either[String, ?]].map(Right(1))(_ + 1)
Functor[Either[String, ?]].map(Left("hi"))((s:String) => s.length)



