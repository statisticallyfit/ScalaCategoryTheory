package functor.data


import cats.data.Validated
import cats.data.Validated.Invalid
import cats.{Eq, Functor, Monoid}



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

     implicit def threeFunctor[A, B] = new Functor[Three[A, B, ?]] {
          def map[C, D](fa: Three[A, B, C])(f: C => D): Three[A, B, D] =
               Three(fa.a, fa.b, f(fa.c))
     }

     implicit def threeEq[A: Eq, B: Eq, C: Eq] = new Eq[Three[A, B, C]] {

          def eqv(three1: Three[A,B,C], three2: Three[A,B,C]): Boolean =
               Eq[A].eqv(three1.a, three2.a) &&
                    Eq[B].eqv(three1.b, three2.b) &&
                    Eq[C].eqv(three1.c, three2.c)
     }
}

// ------------------------------------------------------------------------------------------

class Sum[+B, +A]

object Sum {
     //note: figured out this construction by looking at cats.Validated

     final case class First[+A](first: A) extends Sum[Nothing, A]
     final case class Second[+B](second: B) extends Sum[B, Nothing]

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

class Quant[+A, +B]

object Quant {

     final case class Finance() extends Quant[Nothing, Nothing]
     final case class Desk[+A](desk: A) extends Quant[A, Nothing]
     final case class Bloor[+B](bloor: B) extends Quant[Nothing, B]

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

class Company[+A, +C, +B]

object Company {

     final case class DeepBlue[+A, +C](a: A, c: C) extends Company[A, C, Nothing]
     final case class Something[+B](b: B) extends Company[Nothing, Nothing, B]


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

class Maybe[+A]

object Maybe {

     final case class Nought() extends Maybe[Nothing]
     final case class Just[+A](a: A) extends Maybe[A]


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

class Choice[+B, +A]

object Choice {

     final case class Wrong[+A, +B](a1: A, b: B, a2: A) extends Choice[B, A]
     final case class Right[+B, +A](b1: B, a: A, b2: B) extends Choice[B, A]


     implicit def choiceFunctor[B] = new Functor[Choice[B, ?]] {

          def map[A, C](fa: Choice[B, A])(f: A => C): Choice[B, C] ={
               fa match {
                    case Wrong(a1, b, a2) => Wrong(f(a1), b, f(a2))
                    case Right(b1, a, b2) => Right(b1, f(a), b2)
               }
          }
     }


     implicit def choiceEq[A: Eq, B: Eq] = new Eq[Choice[B, A]] {

          def eqv(c1: Choice[B, A], c2: Choice[B, A]): Boolean ={

               (c1, c2) match {
                    case (Wrong(a1, b, a2), Wrong(a1_, b_, a2_)) =>
                         Eq[A].eqv(a1, a1_) && Eq[A].eqv(a2, a2_) && Eq[B].eqv(b, b_)

                    case (Right(b1, a, b2), Right(b1_, a_, b2_)) =>
                         Eq[B].eqv(b1, b1_) && Eq[B].eqv(b2, b2_) && Eq[A].eqv(a, a_)

                    case (Right(_,_,_), _) => false

                    case (_, Right(_,_,_)) => false
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

class TalkToMe[+A]

object TalkToMe {

     final case class Halt() extends TalkToMe[Nothing]
     final case class Print[+A](string: String, a: A) extends TalkToMe[A]
     final case class Read[+A](reader: String => A) extends TalkToMe[A]


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


class BinaryTree[+A]

object BinaryTree {
     import cats.syntax._

     final case class Branch[+A](left: BinaryTree[A], mid: A, right: BinaryTree[A]) extends BinaryTree[A]
     final case class Leaf[+A](value: A) extends BinaryTree[A]


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

               //val eqTree = implicitly[Eq[BinaryTree[A]]]

               (tree1, tree2) match {
                    case (Leaf(a1), Leaf(a2)) => Eq[A].eqv(a1, a2)

                    case (Branch(l1, m1, r1), Branch(l2, m2, r2)) =>
                         eqv(l1, l2) && Eq[A].eqv(m1, m2) && eqv(r1, r2)

                    case _ => false
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

//note the source is:
// https://github.com/statisticallyfit/Haskell/blob/e5e8e2eaf3dc8e0678691672010d2cc29e3fdb8e/HaskellTutorial/HaskellLearningTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/chapter16_Functor/chapterExercises/set3_writeFunctorInstances/exercise2.hs

class Konstant[+A, +B]

object Konstant {

     final case class Const[+A](a: A) extends Konstant[A, Nothing]


     implicit def constantFunctor[A] = new Functor[Konstant[A, ?]] {

          //note: weird: must say end type is A, C when I don't even apply the function !
          def map[B, C](fa: Konstant[A, B])(f: B => C): Konstant[A, C] ={

               fa match {
                    case Const(a) => Const(a)
               }
          }
     }

     implicit def constantEq[A: Eq, B] = new Eq[Konstant[A, B]] {
          def eqv(c1: Konstant[A, B], c2: Konstant[A, B]): Boolean ={

               (c1, c2) match {
                    case (Const(a1), Const(a2)) => Eq[A].eqv(a1, a2)
               }
          }
     }
}


// ------------------------------------------------------------------------------------------
//todo here need help

//class Flip[F, +A, +B]
//
//object Flip {
//     final case class Flipper[F, +B, +A](f: F, b: B, a: A) extends Flip[F, A, B]
//
//
//     implicit def flipFunctor[A, B] = new Functor[Flip[Konstant[A, B], ?, B]] {
//
//          def map[_, C](fa: Flip[Konstant[A, B], A, B])(f: A => C): Flip[Konstant[C, B], C, B] ={
//
//               import Konstant._
//               fa match {
//                    case Flipper(Const(a1), b, a2) => Flipper(Const(f(a1)), b, f(a2))
//               }
//          }
//     }
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

class OtherKonstant[+A, +B]

object OtherKonstant {
     final case class Const[+B](b: B) extends OtherKonstant[Nothing, B]


     implicit def otherKonstFunctor[A] = new Functor[OtherKonstant[A, ?]] {

          def map[B, C](fa: OtherKonstant[A, B])(f: B => C): OtherKonstant[A, C] ={

               fa match {
                    case Const(b) => Const(f(b))
               }
          }
     }

     implicit def otherKonstEq[A, B: Eq] = new Eq[OtherKonstant[A, B]]{

          def eqv(o1: OtherKonstant[A, B], o2: OtherKonstant[A, B]): Boolean ={

               (o1, o2) match {
                    case (Const(b1), Const(b2)) => Eq[B].eqv(b1, b2)
               }
          }
     }
}

// ------------------------------------------------------------------------------------------

class LiftItOut[F, +A]

object LiftItOut {

     final case class Lift[F, +A](f: F, a: A) extends LiftItOut[F, A]


     implicit def liftFunctor[F: Functor] = new Functor[LiftItOut[F, ?]]{

          def map[A, B](fa: LiftItOut[F, A])(f: A => B): LiftItOut[F, B] ={

               fa match {
                    case Lift()
               }
          }
     }
}