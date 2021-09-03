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
	 * Goal (a) to build a chain Matryoshka from a list of strings. Need a recursive function that will go from top
	 * to down to build our data structure of Matryoshka dolls from the `List` while simultaneously removing the
	 * recursive structure we previously had.
	 * Defining the coalgebra
	 *
	 * Here we define which operation we perform at each step of the computation.
	 *
	 * We have a `List[String]` ==> `Matryoshka[List[String]]`
	 *
	 * How it works: `Coalgebra` determines the next single step of the construction
	 *
	 * `def anamorphism[A](a: A)(f: Coalgebra[F, A])(implicit F: Functor[F]): F[A]`
	 */

	val coalgebraDolls: Coalgebra[Matryoshka, NonEmptyList[String]] = {
		case NonEmptyList(name: String, _: INil[String]) => Tiny(name) //TODO why doesn't Wiem put List[String]
		// instead of INil here?
		case NonEmptyList(name: String, restOfDaughters) => {
			val daughtersList:  List[String] = restOfDaughters.toList

			Doll(name, NonEmptyList.fromSeq(daughtersList.head, daughtersList.tail))
		}
	}


	/**
	 * Goal (b) Calculate number of dolls
	 *
	 * We need to have a function which comes from `Matryoshka[_] => Int`.
	 *
	 * `Algebra[F[_], A]` is a type in the matryoshka library that olds a structure `F` to a value `A` so we can see
	 * it as a function `F[_] => A`.
	 *
	 * `type Algebra[F[_], A] = F[A] => A`
	 *
	 * Defining our `Algebra[Matryoshka, Int]`:
	 *
	 * How it works: we collapse the data structure of Matryoshka dolls into a single integer value while
	 * simultaneously taking the recursive structure and collapsing it to get a single value.
	 *
	 * `def cata[A](f: Algebra[F, A])(implicit BF: Functor[F]): A`
	 */
	val algebraCount: Algebra[Matryoshka, Int] = {
		case Doll(_: String, daughterCount: Int) => 1 + daughterCount  // increment by daughter occurrence found
		case Tiny(_) => 1 // base case need to start from 1 to increment, count 1 doll as 1
	}


	/**
	 * Goal (c) Transform `List[String` to a `List[Person` with its age (the smallest doll should be 6 years old,
	 * and each doll is 1 year older)
	 *
	 * We need `List[String] => Matryoshka[_] => List[Person]`
	 *
	 * `ana { Colagebra[Matryoshka, List[String]] } + cata { Algebra[Matryoshka, List[Person] }`
	 */
	val algebraPersonNameOrder: Algebra[Matryoshka, List[Person]] = {
		case Doll(name: String, daughters: List[Person]) => Person(name, age = daughters.head.age + 1) +: daughters
			//daughters :+ Person(name, age = daughters.last.age + 1)
		case Tiny(name: String) => Person(name, age = 6) :: Nil // just a list not INil from scalaz
	}

	val algebraPersonAgeOrder: Algebra[Matryoshka, List[Person]] = {
		case Doll(name: String, daughters: List[Person]) => daughters :+ Person(name, age = daughters.last.age + 1)
		case Tiny(name: String) => Person(name, age = 6) :: Nil // just a list not INil from scalaz
	}


}







object MatryoshkaExample extends App {

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

	Console.println(s"Example list of dolls via Fix: \n\n$dolls")

	Console.println("\n----------------------------------------------------------------------------\n")




	/**
	 * Goal (a) to build a chain Matryoshka from a list of strings. Need a recursive function that will go from top
	 * to down to build our data structure of Matryoshka dolls from the `List` while simultaneously removing the
	 * recursive structure we previously had.
	 *
	 * Answer: `anamorphism` is a generic function that can build a type of shape `F[_]`
	 *
	 * `def anamorphism[A](a: A)(f: Coalgebra[F, A])(implicit F: Functor[F]): F[A]`
	 */
	val names: NonEmptyList[String] =
		NonEmptyList("Anastasia", "Berenice", "Clara", "Danika", "Eliza", "Olga", "Vasya","Yaroslava", "Zoya")
	//val goalA_ana_namesToMatryoshka: Fix[Matryoshka] = names.ana[Fix[Matryoshka]](coalgebra)


	val anamorphismDolls: Fix[Matryoshka] = Fix(Doll("Anastasia",
		Fix(Doll("Berenice",
			Fix(Doll("Clara",
				Fix(Doll("Danika",
					Fix(Doll("Eliza",
						Fix(Doll("Olga",
							Fix(Doll("Vasya",
								Fix(Doll("Yaroslava",
									Fix(Tiny("Zoya"))
								))
							))
						))
					))
				))
			))
		))
	))

	assert(names.ana[Fix[Matryoshka]](coalgebraDolls) == anamorphismDolls, "Test: goal A anamorphism")

	Console.println(s"Goal A: anamorphism result: \n\n$anamorphismDolls")

	//TODO
	// 1) understand better how the psi and phi functions work in the hylo() definition, in order to create the list
	// of matryoshkas.
	// 2) understand how the composition operator works in the stack frame (recursion).
	// 3) understand what is the purpose of having the Functor doll instance, and how exactly it is used in the
	// anamorphism call.
	// ARTICLE LINK = https://hyp.is/EIgqRAyQEeymfHf30Ljd1A/medium.com/@wiemzin/getting-started-with-recursion-schemes-using-matryoshka-f5b5ec01bb


	Console.println("\n----------------------------------------------------------------------------\n")


	/**
	 * Goal (b) Calculate number of dolls
	 *
	 * We need to have a function which comes from `Matryoshka[_] => Int`.
	 *
	 * `Algebra[F[_], A]` is a type in the matryoshka library that olds a structure `F` to a value `A` so we can see
	 * it as a function `F[_] => A`.
	 *
	 * `type Algebra[F[_], A] = F[A] => A`
	 *
	 * How it works: we need to collapse the matryoshka data structure into a single integer value while
	 * simultaneously taking the recursive structure and collapsing it to get a single value.
	 *
	 * Answer: catamorphism is a from bottom-to-up generic function that can fold a value recursively into a
	 * single value of type `A`
	 *
	 * `def cata[A](f: Algebra[F, A])(implicit BF: Functor[F]): A`
	 */
	//val matryoshkaDolls: Fix[Matryoshka] = anamorphismDolls

	//val goalB_cata_countNumDolls: Int = matryoshkaDolls.cata[Int](algebra)

	assert(anamorphismDolls.cata[Int](algebraCount) == 9, "Test: goal B catamorphism")





	/**
	 * Goal (c) make list of Persons
	 */
	val anaThenCataPersonsNameOrder: List[Person] = names.ana[Fix[Matryoshka]](coalgebraDolls).cata[List[Person]](algebraPersonNameOrder)
	val anaThenCataPersonsAgeOrder: List[Person] = names.ana[Fix[Matryoshka]](coalgebraDolls).cata[List[Person]](algebraPersonAgeOrder)

	val hyloPersonsNameOrder: List[Person] = names.hylo[Matryoshka, List[Person]](algebraPersonNameOrder, coalgebraDolls)

	val hyloPersonsAgeOrder: List[Person] = names.hylo[Matryoshka, List[Person]](algebraPersonAgeOrder, coalgebraDolls)


	assert(anaThenCataPersonsNameOrder == hyloPersonsNameOrder, "Test: ana . cata == hylo (name order)")
	assert(anaThenCataPersonsAgeOrder == hyloPersonsAgeOrder, "Test: ana . cata == hylo (age order)")


	assert(hyloPersonsNameOrder == List(Person("Anastasia",14), Person("Berenice",13),
		Person("Clara",12), Person("Danika",11), Person("Eliza",10),
		Person("Olga",9), Person("Vasya",8), Person("Yaroslava",7),
		Person("Zoya",6)),
		"Test: hylo name order"
	)

	assert(hyloPersonsAgeOrder == List(Person("Zoya",6), Person("Yaroslava",7),
		Person("Vasya",8), Person("Olga",9), Person("Eliza",10),
		Person("Danika",11), Person("Clara",12), Person("Berenice",13),
		Person("Anastasia",14)),
		"Test: hylo age order"
	)

	assert(hyloPersonsNameOrder.reverse == hyloPersonsAgeOrder, "Test: name order reverse of age order")


	Console.println("\n----------------------------------------------------------------------------\n")
}
