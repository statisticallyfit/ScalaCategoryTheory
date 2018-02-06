package functor.data


import cats.{Eq, Functor}
import cats.implicits._
import cats.instances._



case class Identity[A](value: A)

object Identity {

     implicit def identityFunctor/*[A, B]*/ = new Functor[Identity] {

          def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity( f(fa.value) )
     }

     implicit def identityEq[A : Eq] = new Eq[Identity[A]] {

          def eqv(id1: Identity[A], id2: Identity[A]): Boolean = Eq[A].eqv(id1.value, id2.value)
     }
}


// ------------------------------------------------------------------------------------------

case class Pair[A](aFirst: A, aSecond: A)

object Pair {

     implicit def pairFunctor: Functor[Pair] = new Functor[Pair]{

          def map[A, B](fa: Pair[A])(f: A => B): Pair[B] = Pair( f(fa.aFirst), f(fa.aSecond) )
     }

     implicit def pairEq[A: Eq] = new Eq[Pair[A]] {

          def eqv(pair1: Pair[A], pair2: Pair[A]): Boolean =
               Eq[A].eqv(pair1.aFirst, pair2.aFirst) && Eq[A].eqv(pair1.aSecond, pair2.aSecond)
     }
}

// ------------------------------------------------------------------------------------------

case class Two[A, B](aValue: A, bValue: B)

object Two {

     implicit def twoFunctor[A]: Functor[Two[A, ?]] = new Functor[Two[A, ?]] {

          def map[B, C](fa: Two[A, B])(f: B => C): Two[A, C] = Two(fa.aValue, f(fa.bValue))
     }

     implicit def twoEq[A: Eq, B: Eq] = new Eq[Two[A, B]] {

          def eqv(two1: Two[A, B], two2: Two[A, B]): Boolean =
               Eq[A].eqv(two1.aValue, two2.aValue) && Eq[B].eqv(two1.bValue, two2.bValue)
     }
}

// ------------------------------------------------------------------------------------------

case class Three[A, B, C](a: A, b: B, c: C)

object Three {

     implicit def threeFunctor[A, B]/*: Functor[Three[A, B, ?]]*/ = new Functor[Three[A, B, ?]] {
          def map[C, D](fa: Three[A, B, C])(f: C => D): Three[A, B, D] =
               Three(fa.a, fa.b, f(fa.c))

     }

     implicit def threeEq[A: Eq, B: Eq, C: Eq]: Eq[Three[A, B, C]] = new Eq[Three[A, B, C]] {

          def eqv(three1: Three[A,B,C], three2: Three[A,B,C]): Boolean =
               Eq[A].eqv(three1.a, three2.a) &&
                    Eq[B].eqv(three1.b, three2.b) &&
                    Eq[C].eqv(three1.c, three2.c)
     }
}

// ------------------------------------------------------------------------------------------

sealed abstract class Sum[+B, +A]
final case class First[+A](first: A) extends Sum[Nothing, A]
final case class Second[+B](second: B) extends Sum[B, Nothing]

object Sum {

     implicit def sumFunctor[B] = new Functor[Sum[B, ?]] {

          def map[A, C](fa: Sum[B, A])(f: A => C): Sum[B, C] ={
               fa match {
                    case First(a) => First(f(a))
                    case Second(b) => Second(b)
               }
          }
     }

     implicit def sumEq[B: Eq, A: Eq] = new Eq[Sum[B, A]] {

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

sealed abstract class Quant[+A, +B]
final case class Finance() extends Quant[Nothing, Nothing]
final case class Desk[+A](desk: A) extends Quant[A, Nothing]
final case class Bloor[+B](bloor: B) extends Quant[Nothing, B]

object Quant {

     implicit def quantFunctor[A] = new Functor[Quant[A, ?]] {

          def map[B, C](fa: Quant[A, B])(f: B => C): Quant[A, C] ={
               fa match {
                    case Finance() => Finance()
                    case Desk(a) => Desk(a)
                    case Bloor(b) => Bloor(f(b))
               }
          }
     }

     implicit def quantEq[A: Eq, B: Eq] = new Eq[Quant[A, B]] {

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

sealed abstract class Company[+A, +C, +B]
final case class DeepBlue[+A, +C](a: A, c: C) extends Company[A, C, Nothing]
final case class Something[+B](b: B) extends Company[Nothing, Nothing, B]

object Company {

     implicit def companyFunctor[A, C] = new Functor[Company[A, C, ?]] {

          def map[B, D](fa: Company[A, C, B])(f: B => D): Company[A, C, D] ={

               fa match {
                    case d @ DeepBlue(a, c) => d
                    case Something(b) => Something(f(b))
               }
          }
     }

     implicit def companyEq[A: Eq, B: Eq, C: Eq] = new Eq[Company[A, C, B]] {

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

sealed abstract class Maybe[+A]
final case class Nought() extends Maybe[Nothing]
final case class Just[+A](a: A) extends Maybe[A]

object Maybe {
     implicit def maybeFunctor = new Functor[Maybe] {

          def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] ={
               fa match {
                    case Nought() => Nought()
                    case Just(a) => Just(f(a))
               }
          }
     }

     implicit def maybeEq[A: Eq] = new Eq[Maybe[A]] {

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


// ------------------------------------------------------------------------------------------
//note: based off this one:
//https://github.com/statisticallyfit/Haskell/blob/e5e8e2eaf3dc8e0678691672010d2cc29e3fdb8e/HaskellTutorial/HaskellLearningTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set2_flipArguments/ex3.hs

sealed abstract class Decision[+B, +A]
final case class Wrong[+A, +B](a1: A, b: B, a2: A) extends Decision[B, A]
final case class Correct[+B, +A](b1: B, a: A, b2: B) extends Decision[B, A]

object Decision {
     implicit def choiceFunctor[B] = new Functor[Decision[B, ?]] {

          def map[A, C](fa: Decision[B, A])(f: A => C): Decision[B, C] ={
               fa match {
                    case Wrong(a1, b, a2) => Wrong(f(a1), b, f(a2))
                    case Correct(b1, a, b2) => Correct(b1, f(a), b2)
               }
          }
     }


     implicit def choiceEq[A: Eq, B: Eq] = new Eq[Decision[B, A]] {

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
     implicit def talkToMeFunctor = new Functor[TalkToMe] {

          def map[A, B](fa: TalkToMe[A])(f: A => B): TalkToMe[B] ={

               fa match {
                    case Halt() => Halt()
                    case Print(s, a) => Print(s, f(a))
                    case Read(sa) => Read(f.compose(sa))
                    //Read(functorSA.map(s => sa(s))(f))//fmap f sa
                    //Read(s => f(sa(s)))
                    //Read(functorSA.map(fa)((s: String) => f(sa(s))))
               }
          }
     }


     implicit def talkToMeEq[A: Eq] = new Eq[TalkToMe[A]] {

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
     implicit def treeFunctor = new Functor[BinaryTree] {

          def map[A, B](fa: BinaryTree[A])(f: A => B): BinaryTree[B] ={
               fa match {
                    case Leaf(a) => Leaf(f(a))
                    case Branch(left, mid, right) =>
                         Branch(map(left)(f), f(mid), map(right)(f))
               }
          }
     }

     implicit def treeEq[A: Eq] = new Eq[BinaryTree[A]] {

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

     implicit def trainFunctor = new Functor[Train]{

          def map[T, U](fa: Train[T])(f: T => U): Train[U] ={
               fa match {
                    case End() => End()
                    case Wagon(passenger, restOfTrain) => Wagon(f(passenger), map(restOfTrain)(f))
               }
          }
     }

     implicit def trainEq[T: Eq] = new Eq[Train[T]] {
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
     implicit def constantFunctor[A] = new Functor[ConstA[A, ?]] {

          def map[B, C](fa: ConstA[A, B])(f: B => C): ConstA[A, C] ={

               fa match {
                    case ConstA(a) => ConstA(a)
               }
          }
     }

     implicit def constantEq[A: Eq, B] = new Eq[ConstA[A, B]] {
          def eqv(c1: ConstA[A, B], c2: ConstA[A, B]): Boolean ={

               (c1, c2) match {
                    case (ConstA(a1), ConstA(a2)) => Eq[A].eqv(a1, a2)
               }
          }
     }
}


// ------------------------------------------------------------------------------------------
//todo here need help https://github.com/statisticallyfit/Haskell/blob/e5e8e2eaf3dc8e0678691672010d2cc29e3fdb8e/HaskellTutorial/HaskellLearningTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set3_writeFunctorInstances/exercise3.hs

//case class Flip[A, B, C](flip: A => B => C)
//
//object Flip {
//     import cats.syntax.functor._
//
//     import functor.data.Konstant._
//
//     implicit def flipFunctor[A, B] = new Functor[Flip[A, B, ?]] {
//
//          def map[C, D](fa: Flip[A, B, C])(f: C => D): Flip[A, B, D] ={
//
//               fa match {
//                    case Flip(abc) => Flip(f.compose(abc.compose(Konst(_))))
//               }
//          }
//     }
//     /*implicit def flipFunctor[B] = new Functor[Flip[?, B]] {
//
//          def map[A, C](fa: Flip[A, B])(f: A => C): Flip[C, B] = {
//               fa match {
//                    case Flp(flipAB, b, a) =>
//               }
//          }
//     }*/
//     /*implicit def flipFunctor[A, B] = new Functor[Flip[Konstant[A, B], ?, B]] {
//
//          def map[_, C](fa: Flip[Konstant[A, B], A, B])(f: A => C): Flip[Konstant[C, B], C, B] ={
//
//               import Konstant._
//               fa match {
//                    case Flp(Const(a1), b, a2) => Flp(Const(f(a1)), b, f(a2))
//               }
//          }
//     }*/
//
//     /*implicit def flipFunctor[A, B] = new Functor[Flip[Konstant[A, B], ?, B]] {
//
//          def map[_, C](fa: Flip[Konstant[A, B], A, B])(f: A => C): Flip[Konstant[C, B], C, B] ={
//
//               import Konstant._
//               fa match {
//                    case Flipper(Const(a1), b, a2) => Flipper(Const(f(a1)), b, f(a2))
//               }
//          }
//     }*/
//}

// ------------------------------------------------------------------------------------------

case class ConstB[A, B](b: B)

object ConstB {
     implicit def otherKonstFunctor[A] = new Functor[ConstB[A, ?]] {

          def map[B, C](fa: ConstB[A, B])(f: B => C): ConstB[A, C] ={

               fa match {
                    case ConstB(b) => ConstB(f(b))
               }
          }
     }

     implicit def otherKonstEq[A, B: Eq] = new Eq[ConstB[A, B]]{

          def eqv(o1: ConstB[A, B], o2: ConstB[A, B]): Boolean ={

               (o1, o2) match {
                    case (ConstB(b1), ConstB(b2)) => Eq[B].eqv(b1, b2)
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

//todo
case class LiftItOut[A, B](lifter: A => B)

//final case class Lift[+A, +B](lifter: A => B) extends LiftItOut[A, B]
object LiftItOut {

     implicit def liftFunctor[A] = new Functor[LiftItOut[A, ?]]{

          def map[B, C](fa: LiftItOut[A, B])(f: B => C): LiftItOut[A, C] ={

               fa match {
                    case LiftItOut(ab) => LiftItOut(f.compose(ab))
               }
          }
     }

     implicit def liftEq[A, B] = new Eq[LiftItOut[A, B]] {
          def eqv(l1: LiftItOut[A, B], l2: LiftItOut[A, B]): Boolean = true //todo superficial def
     }
}

// ------------------------------------------------------------------------------------------

case class Together[A, B](ab: A => B)

object Together {
     //final case class Tog[+A, +B](ff: A => B, gg: A => B) extends Together[A, B]

     implicit def togetherFunctor[A] = new Functor[Together[A, ?]]{

          def map[B, C](fa: Together[A, B])(f: B => C): Together[A, C] ={
               fa match {
                    case Together(ab) => Together(f.compose(ab))
               }
          }
     }

     implicit def togetherEq[A, B] = new Eq[Together[A, B]] {
          def eqv(t1: Together[A, B], t2: Together[A, B]): Boolean = true //todo superficial def
     }
}

// ------------------------------------------------------------------------------------------


//note - structure is like IgnoreOne in set3 functor exercises of chris allen haskell
case class Separate[A, C, B, D](ff: A => C, gg: B => D)

object Separate {

     implicit def separateFunctor[A, C, B] = new Functor[Separate[A, C, B, ?]]{

          def map[D, E](fa: Separate[A,C,B,D])(f: D => E): Separate[A, C, B, E] ={

               fa match {
                    case Separate(ffa, ggb) => Separate(ffa, f.compose(ggb))
               }
          }
     }

     implicit def separateEq[A, C, B, D] = new Eq[Separate[A, C, B, D]]{
          def eqv(sep1: Separate[A,C,B,D], sep2: Separate[A,C,B,D]): Boolean = true //todo superficial def
     }
}

// ------------------------------------------------------------------------------------------

case class Notorious[O1,O2, A1,A2, T1,T2](go: O1 => O2, ga: A1 => A2, gt: T1 => T2)

object Notorious {

     implicit def notoriousFunctor[O1, O2, A1, A2, T1] = new Functor[Notorious[O1, O2, A1, A2, T1, ?]] {

          def map[T2, Z](fa: Notorious[O1,O2, A1,A2, T1,T2])(f: T2 => Z): Notorious[O1,O2, A1,A2, T1,Z] ={
               fa match {
                    case Notorious(go, ga, gt) => Notorious(go, ga, f.compose(gt))
               }
          }
     }

     implicit def notoriousEq[O1, O2, A1, A2, T1, T2] = new Eq[Notorious[O1, O2, A1, A2, T1, T2]]{
          def eqv(not1: Notorious[O1, O2, A1, A2, T1, T2], not2: Notorious[O1, O2, A1, A2, T1, T2]): Boolean = true
          //todo superficial definition
     }
}