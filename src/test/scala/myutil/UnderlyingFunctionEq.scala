package myutil

import cats.Eq

/**
 *
 */
object UnderlyingFunctionEq {
	//just a whimsical function -- say two functions are equal by default.
	// todo need to fix this???
	implicit def functionEq[A: Eq, B: Eq]: Eq[A => B] = new Eq[A => B] {
		def eqv(f: A => B, g: A => B): Boolean = true //Eq[B].eqv(f(a), g(a))
	}
}
