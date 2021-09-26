import cats.Order
import cats.syntax.order._
import cats.instances.int._

import cats.Functor

import higherkindness.droste._
import data._
import list._
import Basis._

import scala.language.higherKinds

// 2.1 cata

val prodAlg = Algebra[ListF[Int, ?], Int] {
	case NilF => 1
	case ConsF(h, t) => h * t
}

val prod = scheme.cata[ListF[Int, ?], List[Int], Int](prodAlg)