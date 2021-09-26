package RecursionSchemeTutorials.Clayrat_DrosteExploration

/**
 *
 */

import cats.Order
import cats.syntax.order._
import cats.instances.int._

import cats.Functor

import higherkindness.droste._
import data._
import list._
import Basis._

import scala.language.higherKinds


object Sort {

	// 2.1 cata

	val prodAlg: Algebra[ListF[Int, ?], Int] = Algebra[ListF[Int, ?], Int] {
		case NilF => 1
		case ConsF(h, t) => h * t
	}

	val prod: List[Int] => Int = scheme.cata[ListF[Int, ?], List[Int], Int](prodAlg)

}

object SortRunner extends App {

	import Sort._


	assert(prod(List(1,2,3,4,5)) == 120, "Test 1: prod")

}
