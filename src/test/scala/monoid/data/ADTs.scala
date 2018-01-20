package monoid.data

import cats.data.Validated
import cats.data.Validated._
import cats.{Eq, Monoid}

/**
  *
  */



case class Trivial()

object Trivial {
     implicit val trivialMonoid = new Monoid[Trivial]{
          def combine(a: Trivial, b: Trivial): Trivial = Trivial()
          def empty: Trivial = Trivial()
     }
     implicit val trivialEq = new Eq[Trivial]{
          def eqv(a: Trivial, b: Trivial): Boolean = true
     }
}

// ---------------------

case class Two[A, B](a: A, b: B)

object Two {
     implicit def twoMonoid[A, B](implicit monoidA: Monoid[A], monoidB: Monoid[B]) = new Monoid[Two[A, B]]{

          def combine(t1: Two[A, B], t2: Two[A, B]): Two[A, B] =
               Two(monoidA.combine(t1.a, t2.a), monoidB.combine(t1.b, t2.b))

          def empty: Two[A, B] = Two(monoidA.empty, monoidB.empty)
     }

     implicit def twoEq[A, B](implicit eqA: Eq[A], eqB: Eq[B]) = new Eq[Two[A, B]]{
          def eqv(t1: Two[A, B], t2: Two[A, B]): Boolean = eqA.eqv(t1.a, t2.a) && eqB.eqv(t1.b, t2.b)
     }
}

// ---------------------

case class Five[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)

object Five {

     implicit def fiveMonoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid, E: Monoid] = new Monoid[Five[A,B,C,D,E]]{
          def combine(five1: Five[A,B,C,D,E], five2: Five[A,B,C,D,E]) =
               Five(Monoid[A].combine(five1.a, five2.a),
                    Monoid[B].combine(five1.b, five2.b),
                    Monoid[C].combine(five1.c, five2.c),
                    Monoid[D].combine(five1.d, five2.d),
                    Monoid[E].combine(five1.e, five2.e))

          def empty: Five[A,B,C,D,E] =
               Five(Monoid[A].empty,
                    Monoid[B].empty,
                    Monoid[C].empty,
                    Monoid[D].empty,
                    Monoid[E].empty)
     }

     implicit def fiveEq[A: Eq, B: Eq, C: Eq, D: Eq, E: Eq] = new Eq[Five[A,B,C,D,E]]{
          def eqv(five1: Five[A,B,C,D,E], five2: Five[A,B,C,D,E]): Boolean =
               Eq[A].eqv(five1.a, five2.a) &&
                    Eq[B].eqv(five1.b, five2.b) &&
                    Eq[C].eqv(five1.c, five2.c) &&
                    Eq[D].eqv(five1.d, five2.d) &&
                    Eq[E].eqv(five1.e, five2.e)
     }
}


// ---------------------

case class Disjunction(unwrap: Boolean)

object Disjunction {
     implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
          def combine(a: Disjunction, b: Disjunction): Disjunction =
               Disjunction(a.unwrap || b.unwrap)
          def empty: Disjunction = Disjunction(false)
     }

     implicit val disjunctionEq: Eq[Disjunction] = new Eq[Disjunction] {
          def eqv(a: Disjunction, b: Disjunction): Boolean =
               a.unwrap == b.unwrap
     }
}

// ---------------------
case class Conjunction(unwrap: Boolean)

object Conjunction {

     implicit val conjMonoid = new Monoid[Conjunction]{

          def combine(a: Conjunction, b: Conjunction): Conjunction = Conjunction(a.unwrap && b.unwrap)
          def empty: Conjunction = Conjunction(true)
     }

     implicit val conjEq = new Eq[Conjunction] {

          def eqv(a: Conjunction, b: Conjunction): Boolean = a.unwrap == b.unwrap
     }
}


// ---------------------

case class ExclusiveDisjunction(unwrap: Boolean)

object ExclusiveDisjunction{

     implicit val excDisjMonoid = new Monoid[ExclusiveDisjunction]{

          def combine(a: ExclusiveDisjunction, b: ExclusiveDisjunction): ExclusiveDisjunction =
               ExclusiveDisjunction((a.unwrap && !b.unwrap) || (!a.unwrap && b.unwrap))

          def empty: ExclusiveDisjunction = ExclusiveDisjunction(false)
     }

     implicit val excDisjEq = new Eq[ExclusiveDisjunction]{
          def eqv(a: ExclusiveDisjunction, b: ExclusiveDisjunction): Boolean = a.unwrap == b.unwrap
     }
}


// ---------------------

case class ExclusiveNorDisjunction(unwrap: Boolean)

object ExclusiveNorDisjunction{

     implicit val excNorDisjMonoid = new Monoid[ExclusiveNorDisjunction]{

          def combine(a: ExclusiveNorDisjunction, b: ExclusiveNorDisjunction): ExclusiveNorDisjunction =
               ExclusiveNorDisjunction((!a.unwrap || b.unwrap) && (a.unwrap || !b.unwrap))

          def empty: ExclusiveNorDisjunction = ExclusiveNorDisjunction(true)
     }

     implicit val excNorDisjEq = new Eq[ExclusiveNorDisjunction]{
          def eqv(a: ExclusiveNorDisjunction, b: ExclusiveNorDisjunction): Boolean = a.unwrap == b.unwrap
     }
}

// ---------------------

/* //note: not necessary
object SetUnionMonoid {

     implicit def setUnionMonoid[S] = new Monoid[Set[S]]{
          def combine(s: Set[S], t: Set[S]): Set[S] = s.union(t)

          def empty: Set[S] = Set.empty[S]
     }
}*/

// ---------------------

//note: not necessary
//todo how is this implicit instance found? cats.implicits or cats.data or cats.instances?

object ValidatedMonoid {
     implicit def validatedMonoid[E : Monoid, A : Monoid] = new Monoid[Validated[E, A]]{

          def combine(v1: Validated[E, A], v2: Validated[E, A]): Validated[E, A] ={
               (v1, v2) match {
                    case (Invalid(e1), Invalid(e2)) => Invalid(Monoid[E].combine(e1, e2))
                    case (i @ Invalid(_), Valid(_)) => i
                    case (Valid(_), i @ Invalid(_)) => i
                    case (Valid(a1), Valid(a2)) => Valid(Monoid[A].combine(a1, a2))
               }
          }

          def empty: Validated[E, A] = Valid(Monoid[A].empty)
     }
}


// ---------------------

case class AccumulateRight[E, A](validated : Validated[E, A])

object AccumulateRight {

     implicit def accRightMonoid[E: Monoid, A: Monoid] = new Monoid[AccumulateRight[E, A]]{

          def combine(acc1: AccumulateRight[E, A], acc2: AccumulateRight[E, A]): AccumulateRight[E, A] ={

               (acc1, acc2) match {
                    case (AccumulateRight(Valid(a1)), AccumulateRight(Valid(a2))) =>
                         AccumulateRight(Valid(Monoid[A].combine(a1, a2)))

                    case (AccumulateRight(Invalid(e)), _) => AccumulateRight(Invalid(e))

                    case (_, AccumulateRight(Invalid(e))) => AccumulateRight(Invalid(e))
               }
          }

          def empty: AccumulateRight[E, A] = AccumulateRight(Valid(Monoid[A].empty)) //todo what happens if is E empty?
     }


     implicit def accRightEq[E: Eq, A: Eq] = new Eq[AccumulateRight[E, A]]{

          def eqv(acc1: AccumulateRight[E, A], acc2: AccumulateRight[E, A]): Boolean =
               Eq[Validated[E, A]].eqv(acc1.validated, acc2.validated)
     }
}


// ---------------------


case class AccumulateBoth[E, A](validated: Validated[E, A])

object AccumulateBoth {

     implicit def accBothMonoid[E: Monoid, A: Monoid] = new Monoid[AccumulateBoth[E, A]]{

          def combine(acc1: AccumulateBoth[E, A], acc2: AccumulateBoth[E, A]): AccumulateBoth[E, A] ={
               (acc1, acc2) match {
                    case (AccumulateBoth(Valid(a1)), AccumulateBoth(Valid(a2))) =>
                         AccumulateBoth (Valid (Monoid[A].combine(a1, a2)))

                    case (AccumulateBoth(Invalid(e1)), AccumulateBoth(Invalid(e2))) =>
                         AccumulateBoth (Invalid (Monoid[E].combine(e1, e2)))

                    case (_, a @ AccumulateBoth(Invalid(e))) => a

                    case (a @ AccumulateBoth(Invalid(e)), _) => a
               }
          }

          def empty: AccumulateBoth[E, A] = AccumulateBoth(Valid(Monoid[A].empty))
     }


     implicit def accBothEq[E: Eq, A: Eq] = new Eq[AccumulateBoth[E, A]]{
          def eqv(acc1: AccumulateBoth[E, A], acc2: AccumulateBoth[E, A]): Boolean =
               Eq[Validated[E, A]].eqv(acc1.validated, acc2.validated)
     }
}



// ---------------------


//note - useful for other instances like Combine
object FunctionMonoid {
     implicit def functionCompositionMonoid[A: Monoid, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {

          def combine(f: (A => B), g: (A => B)): (A => B) = (a: A) => Monoid[B].combine(f(a), g(a))
          val empty: (A => B) = Function.const(Monoid[B].empty)
     }
}

object FunctionEq {
     //just a whimsical function -- say two functions are equal by default.
     // todo need to fix this???
     implicit def functionEq[A, B]: Eq[A => B] = new Eq[A => B] {
          def eqv(f: A => B, g: A => B): Boolean = true //Eq[B].eqv(f(a), g(a))
     }
}


// ---------------------

case class Combine[A, B](unCombine: A => B)

object Combine {
     /*import FunctionMonoid._
     import FunctionEq._*/

     implicit def combinerMonoid[A, B: Monoid](implicit ev: Monoid[A => B]) = new Monoid[Combine[A, B]]{

          def combine(com1: Combine[A, B], com2: Combine[A, B]): Combine[A, B] ={
               (com1, com2) match {
                    case (Combine(f), Combine(g)) => Combine(Monoid[A => B].combine(f, g))
                         //Combine(Monoid[A => B].combine(f, g))
               }
          }

          def empty: Combine[A, B] = Combine(Monoid[A => B].empty)
     }

     implicit def combineEq[A: Eq, B: Eq](implicit func: Eq[A => B]) = new Eq[Combine[A, B]] {

          def eqv(com1: Combine[A, B], com2: Combine[A, B]): Boolean = Eq[A => B].eqv(com1.unCombine, com2.unCombine)
     }
}


// ---------------------


case class Memory[S, A](runMem: S => (A, S))

object Memory {

     implicit def memoryMonoid[S, A: Monoid] = new Monoid[Memory[S, A]] {

          def combine(mem1: Memory[S, A], mem2: Memory[S, A]): Memory[S, A] = {

               (mem1, mem2) match {
                    case (Memory(f), Memory(g)) => Memory { (s: S) =>

                         val (a, b): (A, S) = g(s)
                         val (c, d): (A, S) = f(b)

                         (Monoid[A].combine(a, c), d)
                    }
               }
          }

          def empty: Memory[S, A] = Memory( (s: S) => (Monoid[A].empty, s))
     }

     //todo need to fix? Just said default "true"
     implicit def memoryEq[S, A]: Eq[Memory[S, A]] = new Eq[Memory[S, A]] {
          def eqv(mem1: Memory[S, A], mem2: Memory[S, A]): Boolean = true
     }
}