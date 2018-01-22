package functor.data


import cats.data.Validated._
import cats.data.Validated

import org.scalacheck.{Arbitrary, Gen}

/**
  *
  */
object ArbitraryADTs {

     implicit def arbIdentity[A: Arbitrary]: Arbitrary[Identity[A]] ={

          val genIdentity: Gen[Identity[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield Identity(a)

          Arbitrary(genIdentity)
     }


     implicit def arbPair[A: Arbitrary]: Arbitrary[Pair[A]] = {

          val genPair: Gen[Pair[A]] = for {
               a1 <- Arbitrary.arbitrary[A]
               a2 <- Arbitrary.arbitrary[A]
          }  yield Pair(a1, a2)

          Arbitrary(genPair)
     }


     implicit def arbTwo[A: Arbitrary, B: Arbitrary]: Arbitrary[Two[A, B]] ={

          val genTwo: Gen[Two[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield Two(a, b)

          Arbitrary(genTwo)
     }

     implicit def arbThree[A: Arbitrary, B: Arbitrary, C: Arbitrary]: Arbitrary[Three[A, B, C]] ={

          val genThree: Gen[Three[A, B, C]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
               c <- Arbitrary.arbitrary[C]
          } yield Three(a, b, c)

          Arbitrary(genThree)
     }

     implicit def arbSum[B: Arbitrary, A: Arbitrary]: Arbitrary[Sum[B, A]] ={
          import functor.data.Sum._

          val genFirst: Gen[First[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield First(a)

          val genSecond: Gen[Second[B]] = for {
               b <- Arbitrary.arbitrary[B]
          } yield Second(b)

          val genSum: Gen[Sum[B, A]] = Gen.oneOf(genFirst, genSecond)

          Arbitrary(genSum)
     }

     implicit def arbQuant[A: Arbitrary, B: Arbitrary]: Arbitrary[Quant[A, B]] ={
          import functor.data.Quant._
          /*val arbA = implicitly[Arbitrary[A]]
          val arbB = implicitly[Arbitrary[B]]*/

          val genFinance: Gen[Finance] = Finance()

          val genDesk: Gen[Desk[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield Desk(a)

          val genBloor: Gen[Bloor[B]] = for {
               b <- Arbitrary.arbitrary[B]
          } yield Bloor(b)

          val genQuant: Gen[Quant[A, B]] = Gen.oneOf(genFinance, genDesk, genBloor)

          Arbitrary(genQuant)

          //todo how to condense the definition?
          //Arbitrary(Gen.oneOf(Finance(), Desk(arbA.arbitrary[A]), Bloor(arbB.arbitrary[B])))
     }


     implicit def arbMaybe[A: Arbitrary]: Arbitrary[Maybe[A]] ={
          import functor.data.Maybe._

          val genNothing: Gen[Nought] = Nought()

          val genJust: Gen[Just[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield Just(a)

          Arbitrary(Gen.oneOf(genNothing, genJust))
     }


     implicit def arbCompany[A: Arbitrary, C: Arbitrary, B: Arbitrary]: Arbitrary[Company[A, C, B]] ={

          import functor.data.Company._

          val genDeepBlue: Gen[DeepBlue[A, C]] = for {
               a <- Arbitrary.arbitrary[A]
               c <- Arbitrary.arbitrary[C]
          } yield DeepBlue(a, c)

          val genSomething: Gen[Something[B]] = for {
               b <- Arbitrary.arbitrary[B]
          } yield Something(b)

          Arbitrary(Gen.oneOf(genDeepBlue, genSomething))
     }

     implicit def arbChoice[A: Arbitrary, B: Arbitrary]: Arbitrary[Choice[B, A]] ={

          import functor.data.Choice._

          val genWrong: Gen[Wrong[A, B]] = for {
               a1 <- Arbitrary.arbitrary[A]
               a2 <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield Wrong(a1, b, a2)

          val genRight: Gen[Right[B, A]] = for {
               b1 <- Arbitrary.arbitrary[B]
               b2 <- Arbitrary.arbitrary[B]
               a <- Arbitrary.arbitrary[A]
          } yield Right(b1, a, b2)

          Arbitrary(Gen.oneOf(genWrong, genRight))
     }


     implicit def arbTalk[A: Arbitrary]: Arbitrary[TalkToMe[A]] ={

          import functor.data.TalkToMe._

          val genHalt: Gen[Halt] = Halt()

          val genPrint: Gen[Print[A]] = for {
               a <- Arbitrary.arbitrary[A]
               str <- Arbitrary.arbitrary[String]
          } yield Print(str, a)

          val genRead: Gen[Read[A]] = for {
               a <- Arbitrary.arbitrary[A]
               str <- Arbitrary.arbitrary[String]
          } yield Read(str => a)

          Arbitrary(Gen.oneOf(genHalt, genPrint, genRead))
          //note: or can have case object Halt extends ...
          // .... and then Gen.const(Halt) instead of genHalt.
     }


     implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] ={

          import functor.data.Tree._

          val genLeaf: Gen[Leaf[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield Leaf(a)

          def genBranch(sz: Int): Gen[Tree[A]] = for {
               n <- Gen.choose(sz/3, sz/2) //min, max
               
          }
          /*val genBranch: Gen[Branch[A]] = for {
               a <- Arbitrary.arbitrary[A]
               left <- genTree
               right <- genTree
          } yield Branch(left, a, right)

          val genTree: Gen[Tree[A]] = Gen.oneOf(genLeaf, genBranch)*/

          Arbitrary
     }
}
