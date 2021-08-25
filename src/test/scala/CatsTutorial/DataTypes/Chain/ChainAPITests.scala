package CatsTutorial.DataTypes.Chain

/**
 * Tutorial source = https://typelevel.org/cats/datatypes/chain.html
 * Chain API Function reference = https://typelevel.org/cats/api/cats/data/Chain.html?search=nonemptychain
 *
 * Other sources for examples:
 * --> http://thushw.blogspot.com/2015/09/scala-collectfirst-example.html
 */

import cats.data._
import cats.kernel.Order

import scala.util.Random


object TestData {


	trait Animal
	case class Mammal(name: String) extends Animal
	case class Bird(name: String) extends Animal

	val animals = Seq(
		Mammal("elephant"),
		Mammal("tiger"),
		Bird("raven"),
		Mammal("monkey"),
		Bird("peacock"),
		Bird("sparrow")
	)

	val birdCollect: PartialFunction[Animal, Bird] = new PartialFunction[Animal, Bird]{
		def isDefinedAt(animal: Animal): Boolean = animal match {
			case Bird(_) => true
			case _ => false
		}

		def apply(animal: Animal): Bird = animal match {
			case b@Bird(_) => b
		}
	}

	val birdCollectSome: Animal => Option[Bird] = (animal: Animal) => animal match {
		case b@Bird(_) => Some(b)
		case _ => None
	}

	val birdRemove: Animal => Boolean = (animal: Animal) => animal match {
		case Bird(_) => true
		case _ => false
	}

	val animalChain: Chain[Animal] = Chain.fromSeq(animals)
	//val temp: Chain[Animal] = Chain(Bird("sparrow"), Bird("sparrow"), Mammal("Deer"), Bird("raven"))
	val animalChainDup: Chain[Animal] = animalChain ++ Chain(Bird("sparrow"), Bird("sparrow"), Mammal("deer"), Bird
	("raven")) //TODO error here chain[object] vs chain[animal]

	implicit def animalOrder: Order[Animal] = new Order[Animal] {
		override def compare(x: Animal, y: Animal): Int = {
			x.hashCode.compare(y.hashCode()) //TODO good practice? Below is too combinational
			/*x match {
				case Bird(xName) => y match {
					case Bird(yName) => xName.compare(yName)
					case Mammal(yName) => xName.compare(yName)
				}
				case Mammal(xName) =>y match {
					case Bird(yName) => xName.compare(yName)
					case Mammal(yName) => xName.compare(yName)
				}
			}*/
		}
	}

	// -----------------------------------------------------------------------------

	val doublesOfPositive = (xs: Chain[Int]) => xs.filter(_ > 0).map(2 * _)
	val doublesOfPositiveCollect = (xs: Chain[Int]) => xs.collect {
		case x if x > 0 => 2 * x
	}
	val doublesOfPositiveCollectFirst = (xs: Chain[Int]) => xs.collectFirst {
		case x if x > 0 => 2 * x
	}

	val xs: Chain[Int] = Chain.fromSeq(Range(-11, 11).toList)

	val values: Chain[Int] = Chain.fromSeq( Random.shuffle(Range(-10, 11).toList) )
}


object ChainAPITests extends App {

	import TestData._



	assert(Chain(1,2) ++ Chain(3, 4) == Chain(1,2,3,4), "Test a: chain concatenation")
	assert(Chain(1,2).concat( Chain(3, 4) ) == Chain(1,2,3,4) , "Test b: chain concatenation")

	assert( 3 +: Chain(3,4,5) == Chain(3,3,4,5), "Test: Prepend" )

	assert( Chain(4,5,6) :+ 19 == Chain(4,5,6,19), "Test: Append" )


	// Collect

	assert(animalChain.collect(birdCollect) == Chain(Bird("raven"), Bird("peacock"), Bird("sparrow")), "Test 1: collect")
	assert(animalChain.collectFirst(birdCollect) == Some(Bird("raven")), "Test 1: collectFirst")


	assert(doublesOfPositive(xs) == doublesOfPositiveCollect(xs)
		&& doublesOfPositiveCollect(xs) == Chain(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), "Test 2: collect")

	assert(doublesOfPositive(xs).headOption == doublesOfPositiveCollectFirst(xs)
		&& doublesOfPositiveCollectFirst(xs) == Some(2), "Test 2: collectFirst")


	// Collect first some
	assert(animalChain.collectFirstSome(birdCollectSome) == animalChain.collectFirst(birdCollect)
		&& animalChain.collectFirstSome(birdCollectSome) == Some(Bird("raven")), "Test 1: collectFirstSome")


	// Delete First
	assert(animalChain.deleteFirst(birdRemove) ==
		Some((Bird("raven"),
		Chain(Mammal("elephant"), Mammal("tiger"), Mammal("monkey"), Bird("peacock"), Bird("sparrow")))),
		"Test 1: Delete first"
	)


	//Distinct (chain)
	assert(animalChainDup.distinct(animalOrder) == (animalChain :+ Mammal("deer")), "Test 1: distinct")


	// FoldLeft (on Chain)
	assert(
		animalChainDup.foldLeft(Bird(""))((accBird, animal) => animal match {
			case Mammal(_) => accBird
			case Bird(incomingBirdName) => Bird(accBird.name + " " + incomingBirdName)
		}) == Bird(" raven peacock sparrow sparrow sparrow raven"),
		"Test 1: foldLeft"
	)

	assert(
		values.foldLeft(0)({case (accPos, num) => if( num < 0) accPos else (accPos + num)}) == 55,
		"Test 2: foldLeft"
	)

	assert(
		Chain.fromSeq(Range(-5, 6).toList).foldLeft("__")((accStr, int) => s"($accStr + $int)")
			== "(((((((((((__ + -5) + -4) + -3) + -2) + -1) + 0) + 1) + 2) + 3) + 4) + 5)",
		"Test 3: foldLeft"
	)


	assert(
		Chain.fromSeq(Range(-5, 6).toList).foldRight("__")((int, accStr) => s"($accStr + $int)")
			== "(((((((((((__ + 5) + 4) + 3) + 2) + 1) + 0) + -1) + -2) + -3) + -4) + -5)",
		"Test 1: foldRight"
	)

	//TODO moving on, left off at groupBy and need to finish downwards:
	// https://typelevel.org/cats/api/cats/data/Chain.html#groupBy[B](f:A=%3EB)(implicitB:cats.Order[B]):scala.collection.immutable.SortedMap[B,cats.data.NonEmptyChain[A]]
}
