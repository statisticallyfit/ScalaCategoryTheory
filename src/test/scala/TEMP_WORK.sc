
/*
import cats.{Eq, Functor}
import cats.implicits._
import functor.data.Three
import functor.data.Three.threeFunctor


//Functor[Three[Int,Int,?]].map(Three(1,1, 2))(_ + 1).map(_ - 5).map(_ * 2)

val t1: Three[Int,Int,Int] = Functor[Three[Int,Int,?]].map(Three(1,1, 2))(_ + 1)
val t2: Three[Int,Int,Int] = Functor[Three[Int,Int,?]].map(t1)(_ - 5)
val t3: Three[Int,Int,Int] = Functor[Three[Int,Int,?]].map(t2)(_ * 2)
*/

import cats.data.Validated
import cats.data.Validated._
import cats.{Eq, Monoid}
import cats.implicits._
import monoid.data._


val LEN = "quicksilver".length

val res2 = Monoid[Memory[String, Int]].combineAll(List(
	Memory((s:String) => (LEN + 2, s"(2 + ${s.length})")),
	Memory((s:String) => (LEN * 4, s"(4 * ($s))")),
	Memory((s:String) => (LEN + 5, s"(5 + ($s))"))
))

print(res2.runMem("quicksilver"))

//res.runMem("quicksilver")
