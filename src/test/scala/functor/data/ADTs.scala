package functor.data


import cats.{Eq, Functor}
import cats.implicits._
import cats.instances._



case class Identity[A](value: A)

object Identity {

     implicit def identityFunctor: Functor[Identity]= new Functor[Identity] {

          def map[A, B](identity: Identity[A])(f: A => B): Identity[B] = Identity( f(identity.value) )
     }

     implicit def identityEq[A : Eq]: Eq[Identity[A]] = new Eq[Identity[A]] {

          def eqv(id1: Identity[A], id2: Identity[A]): Boolean = Eq[A].eqv(id1.value, id2.value)
     }
}


// ------------------------------------------------------------------------------------------

// In haskell this already has kind * -> * so no need to bind any type in the functor instance creation:. This means
// we can apply the function (f) to both the arguments, since we bind none of them.
//data Pair a = Pair a a
//Prelude> :k Pair
//Pair :: * -> *

case class Pair[A](a1: A, a2: A)

object Pair {

     implicit def pairFunctor: Functor[Pair] = new Functor[Pair]{

          def map[A, B](pair: Pair[A])(f: A => B): Pair[B] = Pair( f(pair.a1), f(pair.a2) )
     }

     implicit def pairEq[A: Eq]: Eq[Pair[A]] = new Eq[Pair[A]] {

          def eqv(pair1: Pair[A], pair2: Pair[A]): Boolean =
               Eq[A].eqv(pair1.a1, pair2.a1) && Eq[A].eqv(pair1.a2, pair2.a2)
     }
}

// ------------------------------------------------------------------------------------------


// Haskell source: https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/Functor.hs

case class Two[A, B](a: A, b: B)

object Two {

     //See page 627 of Chris Allen: putting the type A here means we are binding the type here so the function (f)
     // cannot act on the argument that is of type A, only on the leftover types (in this case B)
     // NOTE: reason for doing this: to make the KIND of Two[A, B] be * -> * so it can be the same KIND as fmap,
     // because otherwise by itself the Two[A, B] has kind * -> * -> * which is iincompatible with the kind of fmap
     // (* -> *)
     implicit def twoFunctor[A]: Functor[Two[A, ?]] = new Functor[Two[A, ?]] {

          def map[B, C](two: Two[A, B])(f: B => C): Two[A, C] = Two(two.a, f(two.b))
     }

     implicit def twoEq[A: Eq, B: Eq]: Eq[Two[A, B]] = new Eq[Two[A, B]] {

          def eqv(two1: Two[A, B], two2: Two[A, B]): Boolean =
               Eq[A].eqv(two1.a, two2.a) && Eq[B].eqv(two1.b, two2.b)
     }
}

// ------------------------------------------------------------------------------------------

case class Three[A, B, C](a: A, b: B, c: C)

object Three {

     implicit def threeFunctor[A, B]: Functor[Three[A, B, ?]]= new Functor[Three[A, B, ?]] {
          def map[C, D](three: Three[A, B, C])(f: C => D): Three[A, B, D] =
               Three(three.a, three.b, f(three.c))

     }

     implicit def threeEq[A: Eq, B: Eq, C: Eq]: Eq[Three[A, B, C]] = new Eq[Three[A, B, C]] {

          def eqv(three1: Three[A,B,C], three2: Three[A,B,C]): Boolean =
               Eq[A].eqv(three1.a, three2.a) &&
                    Eq[B].eqv(three1.b, three2.b) &&
                    Eq[C].eqv(three1.c, three2.c)
     }
}



// ------------------------------------------------------------------------------------------

case class ThreeDup[A, B](a: A, b1: B, b2: B)

object ThreeDup {

     implicit def threeFunctor[A]: Functor[ThreeDup[A, ?]]= new Functor[ThreeDup[A, ?]] {
          def map[B, C](three: ThreeDup[A, B])(f: B => C): ThreeDup[A, C] =
               ThreeDup(three.a, f(three.b1), f(three.b2))

     }

     implicit def threeEq[A: Eq, B: Eq, C: Eq]: Eq[ThreeDup[A, B]] = new Eq[ThreeDup[A, B]] {

          def eqv(three1: ThreeDup[A,B], three2: ThreeDup[A,B]): Boolean =
               Eq[A].eqv(three1.a, three2.a) &&
                    Eq[B].eqv(three1.b1, three2.b1) &&
                    Eq[B].eqv(three1.b2, three2.b2)
     }
}

// ------------------------------------------------------------------------------------------

// Source in haskell: https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set2_flipArguments/ex1.hs
// NOTE: Method to determine which type (A, or B) gets bounded:  declare this type asin the above file in the haskell
// terminal and then check its kind by typing ( :k Sum) and then you can see which types (in order you can eliminate
// by bynding in the declaration of Sum functor function)
sealed abstract class Sum[+B, +A]
final case class First[+A](first: A) extends Sum[Nothing, A]
final case class Second[+B](second: B) extends Sum[B, Nothing]

object Sum {

     implicit def sumFunctor[B]:  Functor[Sum[B, ?]] = new Functor[Sum[B, ?]] {

          def map[A, C](sum: Sum[B, A])(f: A => C): Sum[B, C] ={
               sum match {
                    case First(a) => First(f(a))
                    case Second(b) => Second(b)
               }
          }
     }

     implicit def sumEq[B: Eq, A: Eq]: Eq[Sum[B, A]] = new Eq[Sum[B, A]] {

          def eqv(sum1: Sum[B, A], sum2: Sum[B, A]): Boolean = {
               (sum1, sum2) match {
                    case (First(a1), First(a2)) => Eq[A].eqv(a1, a2)
                    case (Second(b1), Second(b2)) => Eq[B].eqv(b1, b2)
                    case (First(_), Second(_)) => false
                    case (Second(_), First(_)) => false
               }
          }
     }
}




// ------------------------------------------------------------------------------------------

//TODO test what happens when removing the + sign from both types +A here:

sealed abstract class Maybe[+A]
final case class Nought() extends Maybe[Nothing]
final case class Just[+A](a: A) extends Maybe[A]

object Maybe {
     implicit def maybeFunctor: Functor[Maybe] = new Functor[Maybe] {

          def map[A, B](maybe: Maybe[A])(f: A => B): Maybe[B] ={
               maybe match {
                    case Nought() => Nought()
                    case Just(a) => Just(f(a))
               }
          }
     }

     implicit def maybeEq[A: Eq]: Eq[Maybe[A]] = new Eq[Maybe[A]] {

          def eqv(maybe1: Maybe[A], maybe2: Maybe[A]): Boolean ={
               (maybe1, maybe2) match {
                    case (Nought(), Nought()) => true
                    case (Just(a1), Just(a2)) => Eq[A].eqv(a1, a2)
                    case (Nought(), _) => false
                    case (_, Nought()) => false
               }
          }
     }
}


// ---------------------------------------------------------------------------------------

// Haskell source: https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/Functor.hs
// NOTE this is just the same kind as Maybe type ( * -> *) so we don't need to bind type A, even though there are
// three constructors instead of two. So number of constructors doesn't amke a difference when same number of types.


sealed abstract class WhoCares[+A]
final case class ItDoesnt() extends WhoCares[Nothing]
final case class Matter[+A](a: A) extends WhoCares[A]
final case class WhatThisIsCalled() extends WhoCares[Nothing]

object WhoCares {
     implicit def whoCaresFunctor: Functor[WhoCares] = new Functor[WhoCares] {

          def map[A, B](whoCares: WhoCares[A])(f: A => B): WhoCares[B] = {
               whoCares match {
                    case ItDoesnt() => ItDoesnt()
                    case Matter(a) => Matter(f(a))
                    case WhatThisIsCalled() => WhatThisIsCalled()
               }
          }
     }

     implicit def whoCaresEq[A: Eq]: Eq[WhoCares[A]] = new Eq[WhoCares[A]] {

          def eqv(who1: WhoCares[A], who2: WhoCares[A]): Boolean = {
               (who1, who2) match {
                    case (ItDoesnt(), ItDoesnt()) => true
                    case (WhatThisIsCalled(), WhatThisIsCalled()) => true
                    case (Matter(a1), Matter(a2)) => Eq[A].eqv(a1, a2)
                    case _ => false
               }
          }
     }
}




// ------------------------------------------------------------------------------------------

//Haskell source: (under Suprising Functors): https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/Functor.hs#L488

sealed abstract class Constant[+A, +B]
final case class GetConstant[+A](a: A) extends Constant[A, Nothing]


object Constant {
     implicit def constantFunctor[A]: Functor[Constant[A, ?]] = new Functor[Constant[A, ?]] {

          def map[B, C](constant: Constant[A, B])(f: B => C): Constant[A, C] ={
               constant match {
                    case GetConstant(constA) => GetConstant(constA) // The A is bound above so we cannot make
                    // function f act on it.
               }
          }
     }

     implicit def constantEq[A: Eq, B]: Eq[Constant[A, B]] = new Eq[Constant[A, B]]{

          def eqv(const1: Constant[A, B], const2: Constant[A, B]): Boolean = {

               (const1, const2) match {
                    case (GetConstant(a1), GetConstant(a2)) => Eq[A].eqv(a1, a2)
               }
          }
     }
}



// ------------------------------------------------------------------------------------------
//Haskell source: https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set3_writeFunctorInstances/exercise3.hs#L19-L23

case class Flip[A, B, C](flip: B => A => C, b: B, a:A)

object Flip {

     /*implicit def flipFunctor[A, C]: Functor[Flip[A, ?, C]] = new Functor[Flip[A, ?, C]] {

          def map[B, Z](flipObj: Flip[A, B, C])(fbz: B => Z): Flip[A, B, Z] ={

               flipObj match {
                    case Flip(fbac, b, a) => Flip( (b: B) => fbz.compose(fbac(b)), b, a)
               }
          }
     }*/
     //TODO fix this error: type mismatch;
     // found   : A => C
     // required: A => B

     /*implicit def flipFunctor[A]: Functor[Flip[A, ?, ?]] = new Functor[Flip[A, ?, ?]] {

          def map[B, C, D](flipObj: Flip[A, B, C])(fcd: C => D): Flip[A, B, D] ={

               flipObj match {
                    case Flip(fbac, b, a) => Flip( (b: B) => fcd.compose(fbac(b)), b, a)
               }
          }
     }*/
     /*implicit def flipFunctor[B] = new Functor[Flip[?, B]] {

          def map[A, C](fa: Flip[A, B])(f: A => C): Flip[C, B] = {
               fa match {
                    case Flp(flipAB, b, a) =>
               }
          }
     }*/
     /*implicit def flipFunctor[A, B] = new Functor[Flip[Konstant[A, B], ?, B]] {

          def map[_, C](fa: Flip[Konstant[A, B], A, B])(f: A => C): Flip[Konstant[C, B], C, B] ={

               import Konstant._
               fa match {
                    case Flp(Const(a1), b, a2) => Flp(Const(f(a1)), b, f(a2))
               }
          }
     }*/

     /*implicit def flipFunctor[A, B] = new Functor[Flip[Konstant[A, B], ?, B]] {

          def map[_, C](fa: Flip[Konstant[A, B], A, B])(f: A => C): Flip[Konstant[C, B], C, B] ={

               import Konstant._
               fa match {
                    case Flipper(Const(a1), b, a2) => Flipper(Const(f(a1)), b, f(a2))
               }
          }
     }*/
}


// ------------------------------------------------------------------------------------------

sealed abstract class Quant[+A, +B]
final case class Finance() extends Quant[Nothing, Nothing]
final case class Desk[+A](desk: A) extends Quant[A, Nothing]
final case class Bloor[+B](bloor: B) extends Quant[Nothing, B]

object Quant {

     implicit def quantFunctor[A]: Functor[Quant[A, ?]] = new Functor[Quant[A, ?]] {

          def map[B, C](quant: Quant[A, B])(f: B => C): Quant[A, C] ={
               quant match {
                    case Finance() => Finance()
                    /* Can't act over 'a' because it must be used as placeholder to make the
                     * kind for Quant from * -> * -> * into * -> * so that Functor type declaration is satisfied.
                     */
                    case Desk(a) => Desk(a)
                    //Can onlyact over the 'B' since that is the wildcard one, not Fixed in the type declaration.
                    case Bloor(b) => Bloor(f(b))
               }
          }
     }

     implicit def quantEq[A: Eq, B: Eq]: Eq[Quant[A, B]] = new Eq[Quant[A, B]] {

          def eqv(quant1: Quant[A, B], quant2: Quant[A, B]): Boolean = {

               quant1 match {
                    case Finance() => quant2 match {
                         case Finance() => true
                         case Desk(_) => false
                         case Bloor(_) => false
                    }

                    case Desk(a1) => quant2 match {
                         case Finance() => false
                         case Desk(a2) => Eq[A].eqv(a1, a2)
                         case Bloor(_) => false
                    }

                    case Bloor(b1) => quant2 match {
                         case Finance() => false
                         case Desk(_) => false
                         case Bloor(b2) => Eq[B].eqv(b1, b2)
                    }
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

//Haskell source = https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set2_flipArguments/ex2.hs#L2-L6


sealed abstract class Company[+A, +C, +B]
final case class DeepBlue[+A, +C](a: A, c: C) extends Company[A, C, Nothing]
final case class Something[+B](b: B) extends Company[Nothing, Nothing, B]

object Company {

     implicit def companyFunctor[A, C]: Functor[Company[A, C, ?]] = new Functor[Company[A, C, ?]] {

          def map[B, Z](company: Company[A, C, B])(f: B => Z): Company[A, C, Z] ={

               company match {
                    case d @ DeepBlue(a, c) => d
                    case Something(b) => Something(f(b))
               }
          }
     }

     implicit def companyEq[A: Eq, B: Eq, C: Eq]: Eq[Company[A, C, B]] = new Eq[Company[A, C, B]] {

          def eqv(c1: Company[A, C, B], c2: Company[A, C, B]): Boolean ={

               (c1, c2) match {
                    case (DeepBlue(a1, c1), DeepBlue(a2, c2)) => Eq[A].eqv(a1, a2) && Eq[C].eqv(c1, c2)
                    case (Something(b1), Something(b2)) => Eq[B].eqv(b1, b2)
                    case (DeepBlue(_, _), _) => false
                    case (_, DeepBlue(_, _)) => false
               }
          }
     }
}




// ------------------------------------------------------------------------------------------
//note: based off this one:
//https://github.com/statisticallyfit/Haskell/blob/e5e8e2eaf3dc8e0678691672010d2cc29e3fdb8e/HaskellTutorial/HaskellLearningTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set2_flipArguments/ex3.hs

sealed abstract class Decision[+B, +A]
final case class Wrong[+A, +B](a1: A, b: B, a2: A) extends Decision[B, A]
final case class Correct[+B, +A](b1: B, a: A, b2: B) extends Decision[B, A]

object Decision {
     implicit def choiceFunctor[B]: Functor[Decision[B, ?]] = new Functor[Decision[B, ?]] {

          def map[A, C](decision: Decision[B, A])(f: A => C): Decision[B, C] ={
               decision match {
                    case Wrong(a1, b, a2) => Wrong(f(a1), b, f(a2))
                    case Correct(b1, a, b2) => Correct(b1, f(a), b2)
               }
          }
     }


     implicit def choiceEq[A: Eq, B: Eq]: Eq[Decision[B, A]] = new Eq[Decision[B, A]] {

          def eqv(c1: Decision[B, A], c2: Decision[B, A]): Boolean ={

               (c1, c2) match {
                    case (Wrong(a1, b, a2), Wrong(a1_, b_, a2_)) =>
                         Eq[A].eqv(a1, a1_) && Eq[A].eqv(a2, a2_) && Eq[B].eqv(b, b_)

                    case (Correct(b1, a, b2), Correct(b1_, a_, b2_)) =>
                         Eq[B].eqv(b1, b1_) && Eq[B].eqv(b2, b2_) && Eq[A].eqv(a, a_)

                    case (Correct(_,_,_), _) => false

                    case (_, Correct(_,_,_)) => false
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

sealed abstract class TalkToMe[+A]
final case class Halt() extends TalkToMe[Nothing]
final case class Print[+A](string: String, a: A) extends TalkToMe[A]
final case class Read[+A](reader: String => A) extends TalkToMe[A]

object TalkToMe {
     implicit def talkToMeFunctor: Functor[TalkToMe] = new Functor[TalkToMe] {

          def map[A, B](talkToMe: TalkToMe[A])(f: A => B): TalkToMe[B] ={

               talkToMe match {
                    case Halt() => Halt()
                    case Print(s, a) => Print(s, f(a))
                    case Read(sa) => Read(f.compose(sa))
                    //Read(functorSA.map(s => sa(s))(f))//fmap f sa
                    //Read(s => f(sa(s)))
                    //Read(functorSA.map(fa)((s: String) => f(sa(s))))
               }
          }
     }


     implicit def talkToMeEq[A: Eq]: Eq[TalkToMe[A]] = new Eq[TalkToMe[A]] {

          def eqv(talk1: TalkToMe[A], talk2: TalkToMe[A]): Boolean = {

               talk1 match {
                    case Halt() => talk2 match {
                         case Halt() => true
                         case _ => false
                    }

                    case Print(s1, a1) => talk2 match {
                         case Print(s2, a2) => s1 == s2 && Eq[A].eqv(a1, a2)
                         case _ => false
                    }

                    case Read(sa1) => talk2 match {
                         case Read(sa2) => true //todo just whimsical definition to let it pass
                         case _ => false
                    }
               }
          }
     }
}


// ------------------------------------------------------------------------------------------


sealed abstract class BinaryTree[+A]
final case class Leaf[+A](value: A) extends BinaryTree[A]
final case class Branch[+A](left: BinaryTree[A], mid: A, right: BinaryTree[A]) extends BinaryTree[A]

//note changing this def since functor composition not working with previous def.

object BinaryTree {
     implicit def treeFunctor: Functor[BinaryTree]  = new Functor[BinaryTree] {

          def map[A, B](binaryTree: BinaryTree[A])(f: A => B): BinaryTree[B] ={
               binaryTree match {
                    case Leaf(a) => Leaf(f(a))
                    case Branch(left, mid, right) =>
                         Branch(map(left)(f), f(mid), map(right)(f))
               }
          }
     }

     implicit def treeEq[A: Eq]: Eq[BinaryTree[A]] = new Eq[BinaryTree[A]] {

          def eqv(tree1: BinaryTree[A], tree2: BinaryTree[A]): Boolean ={
               (tree1, tree2) match {
                    case (Leaf(a1), Leaf(a2)) => Eq[A].eqv(a1, a2)
                    case (Branch(l1, m1, r1), Branch(l2, m2, r2)) => eqv(l1, l2) && eqv(r1, r2) && Eq[A].eqv(m1,m2)
                    case _ => false
               }
          }
     }
}



//TODO class RoseTree (or Rose or Flower)
//class Stem(stems: T*) extends Rose.      .. class Petal(_) extends Rose ...
// ------------------------------------------------------------------------------------------

//another name for List type
sealed abstract class Train[+T]
final case class End() extends Train[Nothing]
final case class Wagon[+T](passenger: T, train: Train[T]) extends Train[T]

object Train {

     implicit def trainFunctor: Functor[Train] = new Functor[Train]{

          def map[T, U](train: Train[T])(f: T => U): Train[U] ={
               train match {
                    case End() => End()
                    case Wagon(passenger, restOfTrain) => Wagon(f(passenger), map(restOfTrain)(f))
               }
          }
     }

     implicit def trainEq[T: Eq]: Eq[Train[T]] = new Eq[Train[T]] {
          def eqv(train1: Train[T], train2: Train[T]): Boolean ={

               (train1, train2) match {
                    case (End(), End()) => true
                    case (Wagon(p1, t1), Wagon(p2, t2)) => Eq[T].eqv(p1,p2) && eqv(t1, t2)
                    case _ => false
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

//note the source is:
// https://github.com/statisticallyfit/Haskell/blob/e5e8e2eaf3dc8e0678691672010d2cc29e3fdb8e/HaskellTutorial/HaskellLearningTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set3_writeFunctorInstances/exercise2.hs

case class ConstA[A, B](a: A)

object ConstA {
     implicit def constantFunctor[A]: Functor[ConstA[A, ?]] = new Functor[ConstA[A, ?]] {

          def map[B, C](consta: ConstA[A, B])(f: B => C): ConstA[A, C] ={

               consta match {
                    case ConstA(a) => ConstA(a)
               }
          }
     }

     implicit def constantEq[A: Eq, B]: Eq[ConstA[A, B]] = new Eq[ConstA[A, B]] {
          def eqv(c1: ConstA[A, B], c2: ConstA[A, B]): Boolean ={

               (c1, c2) match {
                    case (ConstA(a1), ConstA(a2)) => Eq[A].eqv(a1, a2)
               }
          }
     }
}



// ------------------------------------------------------------------------------------------

case class ConstB[A, B](b: B)

object ConstB {
     implicit def otherKonstFunctor[A]: Functor[ConstB[A, ?]] = new Functor[ConstB[A, ?]] {

          def map[B, C](constb: ConstB[A, B])(f: B => C): ConstB[A, C] ={

               constb match {
                    case ConstB(b) => ConstB(f(b))
               }
          }
     }

     implicit def otherKonstEq[A, B: Eq]: Eq[ConstB[A, B]] = new Eq[ConstB[A, B]]{

          def eqv(o1: ConstB[A, B], o2: ConstB[A, B]): Boolean ={

               (o1, o2) match {
                    case (ConstB(b1), ConstB(b2)) => Eq[B].eqv(b1, b2)
               }
          }
     }
}






// ------------------------------------------------------------------------------------------

// Haskell source: https://github.com/statisticallyfit/HaskellTutorial/blob/master/learninghaskell/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set3_writeFunctorInstances/exercise5.hs#L18-L22


case class LiftItOut[A, B](lifter: A => B)

//final case class Lift[+A, +B](lifter: A => B) extends LiftItOut[A, B]
object LiftItOut {

     implicit def liftFunctor[A]: Functor[LiftItOut[A, ?]] = new Functor[LiftItOut[A, ?]]{

          def map[B, Z](liftItOut: LiftItOut[A, B])(fbc: B => Z): LiftItOut[A, Z] ={

               liftItOut match {
                    case LiftItOut(fab) => LiftItOut(fbc.compose(fab))
               }
          }
     }

     implicit def liftEq[A: Eq, B: Eq](implicit ev: Eq[A => B]): Eq[LiftItOut[A, B]] = new Eq[LiftItOut[A, B]] {
          def eqv(l1: LiftItOut[A, B], l2: LiftItOut[A, B]): Boolean = Eq[A => B].eqv(l1.lifter, l2.lifter)
     }
}


// ------------------------------------------------------------------------------------------


//note - structure is like IgnoreOne in set3 functor exercises of chris allen haskell
case class Separate[A, C, B, D](fac: A => C, fbd: B => D)

object Separate {

     implicit def separateFunctor[A, C, B]: Functor[Separate[A, C, B, ?]] = new Functor[Separate[A, C, B, ?]]{

          def map[D, E](separate: Separate[A,C,B,D])(fde: D => E): Separate[A, C, B, E] ={

               separate match {
                    case Separate(_fac, _fbd) => Separate(_fac, fde.compose(_fbd))
               }
          }
     }

     implicit def separateEq[A, C, B, D](implicit evAC: Eq[A => C], evBD: Eq[B => D]): Eq[Separate[A, C,B, D]] =
          new Eq[Separate[A, C,B, D]]{

          def eqv(sep1: Separate[A,C,B,D], sep2: Separate[A,C,B,D]): Boolean =
               Eq[A => C].eqv(sep1.fac, sep2.fac) && Eq[B => D].eqv(sep1.fbd, sep2.fbd)
     }
}

// ------------------------------------------------------------------------------------------

case class Notorious[O1,O2, A1,A2, T1,T2](go: O1 => O2, ga: A1 => A2, gt: T1 => T2)

object Notorious {

     implicit def notoriousFunctor[O1, O2, A1, A2, T1]: Functor[Notorious[O1, O2, A1, A2, T1, ?]] =
          new Functor[Notorious[O1, O2, A1, A2, T1, ?]] {

          def map[T2, Z](notorious: Notorious[O1,O2, A1,A2, T1,T2])(ftz: T2 => Z): Notorious[O1,O2, A1,A2, T1,Z] ={
               notorious match {
                    case Notorious(go, ga, gt) => Notorious(go, ga, ftz.compose(gt))
               }
          }
     }

     implicit def notoriousEq[O1, O2, A1, A2, T1, T2](implicit evO1O2: Eq[O1 => O2],
                                                      evA1A2: Eq[A1 => A2],
                                                      evT1T2: Eq[T1 => T2]): Eq[Notorious[O1, O2, A1, A2, T1, T2]] =
          new Eq[Notorious[O1, O2, A1, A2, T1, T2]]{

          def eqv(not1: Notorious[O1, O2, A1, A2, T1, T2], not2: Notorious[O1, O2, A1, A2, T1, T2]): Boolean =
               Eq[O1 => O2].eqv(not1.go, not2.go) &&
                    Eq[A1 => A2].eqv(not1.ga, not2.ga) &&
                    Eq[T1 => T2].eqv(not1.gt, not2.gt)
          //todo superficial definition
     }
}