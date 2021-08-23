package CatsTutorial.DataTypes

/**
 * Tutorial source = https://typelevel.org/cats/datatypes/chain.html
 */
import cats.implicits._
import cats.instances._
import cats.syntax._
import cats.data.NonEmptyChain


object ChainExample extends App {

	/*
	sealed abstract class EChain[+A]

	case object Empty extends EChain[Nothing]
	case class Singleton[A](a: A) extends EChain[A]
	// Append gives fast concatenation ability (always constant time O(1))
	// NOTE: not exposed for user usage because then they could append empty chain or seq
	case class Append[A](left: EChain[A], right: EChain[A]) extends EChain[A]

	//Lifts any Seq into a Chain
	case class Wrap[A](seq: Seq[A]) extends EChain[A]

	//def isEmpty[A](c: EChain[A]): Boolean = c == Empty

	// Chain does not directly expose Append and Wrap because arguments might be empty Chain or Seq, instead use
	// concat or ++
	def concat[A](c: EChain[A], c2: EChain[A]): EChain[A] = {
		if (c.isEmpty) c2
		else if (c2.isEmpty) c
		else Append(c, c2)
	}
	 */

	println(NonEmptyChain(1,2,3,4))
}
