package RecursionSchemeTutorials.WiemZineElabadine_GettingStartedMatryoshka


//import cats.data.NonEmptyList

//import cats.{Eq, Functor}
//import cats.implicits._
//import cats.instances._

import scalaz._
import scalaz.NonEmptyList
import slamdata.Predef._
import scalaz.Functor

import scala.language.higherKinds

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

/*
 * Goals:
	a) Build Matryoshka dolls from a list of names
	b) Calculate the number of dolls
	c) Transform List[String] to a List[Person] with its age (the smallest doll should be 6 years old, and each doll
	* is 1 year older)
 */

case class Person(name: String, age: Int)



/**
 * This version of Matryoshka uses a generic recursion approach, but we want to evaluate our data structure to other
 * values of any type we want, so we need a generic parameter type and replace the recursion reference
 *
 *
 */

//sealed trait Matryoshka
//case class Doll(name: String, daughter: Matryoshka) extends Matryoshka
//case class Tiny(name: String) extends Matryoshka


/**
 * If we want to build a recursive Data Structure from any value,
 * we need to define: `T` as `Matryoshka[T]` where `T` is another `Matryoshka[T]` ..
 * If we have 10 dolls: `Matryoshka[Matryoshka[Matryoshka[Matryoshka[Matryoshka[Matryoshka[Matryoshka[Matryoshka[Matryoshka[Matryoshka[Nothing]]]]]]]]]]`
 *
 */

sealed trait Matryoshka[T]
case class Doll[T](name: String, daughter: T) extends Matryoshka[T]
case class Tiny[T](name: String) extends Matryoshka[T]

/**
 * How to generalize this recursion? We need to define `T` as another `Matryoshka[T]`: so that
 * `T = Matryoshka[T]`
 *
 * This reminds us about the fixed point of a function: f(x) = x
 * So similarly we need a fixed Point of the type `Matryoshka[_]`
 *
 * `case class Fix[F[_]](unFix: F[Fix[F]])`
 */


/**
 * This allows us to recapture recursion: `T = Fix[Matryoshka]`
 */


// Need Functor instance to make recursion schemes work
object Matryoshka {
	implicit def dollsFunctorImpl: Functor[Matryoshka] = new Functor[Matryoshka] {
		override def map[A, B](fa: Matryoshka[A])(f: A => B): Matryoshka[B] = fa match {
			case Doll(name, daughter) => Doll(name, f(daughter))
			case Tiny(name) => Tiny(name)
		}
	}

	/**
	 * Starting with recursion schemes:
	 *
	 * First goal: "Build Matryohska dolls from a list of names" (nonempty list)
	 *
	 * Need to have a function which comes from `NonEmptyList[String] => Matryoshka[_]`
	 *
	 * Answer: `Coalgebra[F[_], A]` is a type in the matryoshka library that unfolds a value A
	 * to a structure `F[_]`, and we can see it as a function: `A => F[_]`
	 *
	 * `type Coalgebra[F[_], A] = A => F[A]`
	 *
	 *
	 */

	// F[_] = Matryoshka[_]
	// A = NonEmptyList[String]


	/**
	 * Defining the coalgebra
	 *
	 * Here we define which operation we perform at each step of the computation. We have a `List[String]` as an
	 * input and we will return `Matryoshka[List[String]]`
	 *
	 * How it works: `Coalgebra` determines the next single step of the construction
	 */

	val coalgebra: Coalgebra[Matryoshka, NonEmptyList[String]] = {
		case NonEmptyList(name: String, _: INil[String]) => Tiny(name) //TODO why doesn't Wiem put List[String]
		// instead of INil here?
		case NonEmptyList(name: String, restOfDaughters) => {
			val daughtersList:  List[String] = restOfDaughters.toList

			Doll(name, NonEmptyList.fromSeq(daughtersList.head, daughtersList.tail))
		}
	}





}







object Main extends App {

	import Matryoshka._

	val dolls: Fix[Matryoshka] = Fix(
		Doll(
			"Anna",
			Fix(
				Doll(
					"Lisa",
					Fix(
						Tiny("Kate")
					)
				)
			)
		)
	)

	Console.println(dolls)

	// ----------------------------------------------------------------------------



	/**
	 * Goal (a) to build a chain Matryoshka from a list of strings. Need a recursive function that will go from top
	 * to down to build our data structure of Matryoshka dolls from the `List` while simultaneously removing the
	 * recursive structure we previously had.
	 *
	 * Answer: `anamorphism`is a generic function that can build a type of shape `F[_]`
	 *
	 * `def anamorphism[A](a: A)(f: Coalgebra[F, A])(implicit F: Functor[F]): F[A]`
	 */
	val names: NonEmptyList[String] =
		NonEmptyList("Anastasia", "Berenice", "Clara", "Danika", "Eliza", "Olga", "Vasya","Yaroslava", "Zoya")
	val goalA_namesToMatryoshka: Fix[Matryoshka] = names.ana[Fix[Matryoshka]](coalgebra)

	Console.println(goalA_namesToMatryoshka)
}
