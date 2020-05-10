
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
Monoid[ExclusiveNorDisjunction].combineAll(
	List(ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false))) == ExclusiveNorDisjunction(true)

//Even TRUES, Odd FALSE
Monoid[ExclusiveNorDisjunction].combineAll(
	List(ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(false))) == ExclusiveNorDisjunction(false)

//Odd TRUES, Odd False
Monoid[ExclusiveNorDisjunction].combineAll(
	List(ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false))) == ExclusiveNorDisjunction(false)

//Odd TRUES, Even False
Monoid[ExclusiveNorDisjunction].combineAll(
	List(ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(true),
		ExclusiveNorDisjunction(false),
		ExclusiveNorDisjunction(false))) == ExclusiveNorDisjunction(true)