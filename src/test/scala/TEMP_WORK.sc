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

import cats.{Eq, Monoid}
import cats.implicits._
import monoid.data._
Monoid[ExclusiveDisjunction].combineAll(
	List(ExclusiveDisjunction(true),
		ExclusiveDisjunction(false),
		ExclusiveDisjunction(false),
		ExclusiveDisjunction(true),
		ExclusiveDisjunction(true),
		ExclusiveDisjunction(false),
		ExclusiveDisjunction(false)))