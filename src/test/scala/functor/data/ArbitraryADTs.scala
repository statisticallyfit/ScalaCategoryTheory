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

          val genNothing: Gen[Nought] = Nought()

          val genJust: Gen[Just[A]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield Just(a)

          Arbitrary(Gen.oneOf(genNothing, genJust))
     }


     implicit def arbCompany[A: Arbitrary, C: Arbitrary, B: Arbitrary]: Arbitrary[Company[A, C, B]] ={

          val genDeepBlue: Gen[DeepBlue[A, C]] = for {
               a <- Arbitrary.arbitrary[A]
               c <- Arbitrary.arbitrary[C]
          } yield DeepBlue(a, c)

          val genSomething: Gen[Something[B]] = for {
               b <- Arbitrary.arbitrary[B]
          } yield Something(b)

          Arbitrary(Gen.oneOf(genDeepBlue, genSomething))
     }

     implicit def arbChoice[A: Arbitrary, B: Arbitrary]: Arbitrary[Decision[B, A]] ={

          val genWrong: Gen[Wrong[A, B]] = for {
               a1 <- Arbitrary.arbitrary[A]
               a2 <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield Wrong(a1, b, a2)

          val genRight: Gen[Correct[B, A]] = for {
               b1 <- Arbitrary.arbitrary[B]
               b2 <- Arbitrary.arbitrary[B]
               a <- Arbitrary.arbitrary[A]
          } yield Correct(b1, a, b2)

          Arbitrary(Gen.oneOf(genWrong, genRight))
     }


     implicit def arbTalk[A: Arbitrary]: Arbitrary[TalkToMe[A]] ={

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


     implicit def arbTree[A: Arbitrary]: Arbitrary[BinaryTree[A]] ={

          val genLeaf: Gen[Leaf[A]] = for {
               a <- Arbitrary.arbitrary[A] suchThat (_ != null)
          } yield Leaf(a)

          def genBranch: Gen[Branch[A]] = for {
               //a <- Arbitrary.arbitrary[A] suchThat (_ != null)
               left <- Gen.oneOf(genLeaf, genBranch) suchThat (_ != null)
               right <- Gen.oneOf(genLeaf, genBranch) suchThat (_ != null)
          } yield Branch(left, /*a,*/ right)

          val genTree: Gen[BinaryTree[A]] = Gen.oneOf(genLeaf, genBranch)

          Arbitrary(genTree)
     }

     implicit def arbTree2[A: Arbitrary]: Arbitrary[BinaryTree2[A]] ={

          val genLeaf2 = Gen.const(Leaf2)

          def genBranch2: Gen[Branch2[A]] = for {
               a <- Arbitrary.arbitrary[A] suchThat (_ != null)
               left <- Gen.sized(h => Gen.resize(h/2, genTree2)) suchThat (_ != null)
               right <- Gen.sized(h => Gen.resize(h/2, genTree2)) suchThat (_ != null)
          } yield Branch2(left, a, right)

          def genTree2: Gen[BinaryTree2[A]] = Gen.sized { height =>
               if(height <= 0) {
                    genLeaf2
               } else {
                    Gen.oneOf(genLeaf2, genBranch2)
               }
          }

          Arbitrary(genTree2)
          /*val genLeaf2: Gen[Leaf2.type] = Gen.const(Leaf2) suchThat (_ != null)

          def genBranch2: Gen[Branch2[A]] = for {
               a <- Arbitrary.arbitrary[A] suchThat (_ != null)
               left <- Gen.oneOf(genLeaf2, genBranch2) suchThat (_ != null)
               right <- Gen.oneOf(genLeaf2, genBranch2) suchThat (_ != null)
          } yield Branch2(left, a, right)


          Arbitrary(Gen.oneOf(genLeaf2, genBranch2))*/
     }


     implicit def arbTrain[T: Arbitrary]: Arbitrary[Train[T]] ={

          val genEnd: Gen[End] = End()

          def genWagon: Gen[Wagon[T]] = for {
               person <- Arbitrary.arbitrary[T] suchThat (_ != null)
               leftoverTrain <- Gen.oneOf(genEnd, genWagon) suchThat (_ != null)
          } yield Wagon(person, leftoverTrain)

          Arbitrary(Gen.oneOf(genEnd, genWagon))
     }


     implicit def arbConstant[A: Arbitrary, B]: Arbitrary[ConstA[A, B]] ={
          val genConst: Gen[ConstA[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
          } yield ConstA(a)

          Arbitrary(genConst)
     }

     implicit def arbOtherConstant[A, B: Arbitrary]: Arbitrary[ConstB[A, B]] ={
          val genConst: Gen[ConstB[A, B]] = for {
               b <- Arbitrary.arbitrary[B]
          } yield ConstB(b)

          Arbitrary(genConst)
     }

     implicit def arbLiftItOut[A: Arbitrary, B: Arbitrary]: Arbitrary[LiftItOut[A, B]] ={

          val genLiftItOut: Gen[LiftItOut[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield LiftItOut(a => b)

          Arbitrary(genLiftItOut)
     }

     implicit def arbTogether[A: Arbitrary, B: Arbitrary]: Arbitrary[Together[A, B]] ={

          val genTogether: Gen[Together[A, B]] = for {
               a <- Arbitrary.arbitrary[A]
               b <- Arbitrary.arbitrary[B]
          } yield Together(a => b)

          Arbitrary(genTogether)
     }

     implicit def arbSeparate[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]: Arbitrary[Separate[A,C,B,D]] ={

          val genSeparate: Gen[Separate[A,C,B,D]] = for {
               a <- Arbitrary.arbitrary[A]
               c <- Arbitrary.arbitrary[C]
               b <- Arbitrary.arbitrary[B]
               d <- Arbitrary.arbitrary[D]
          } yield Separate(a => c, b => d)

          Arbitrary(genSeparate)
     }

     implicit def arbNotorious[O1: Arbitrary, O2: Arbitrary, A1: Arbitrary, A2: Arbitrary, T1: Arbitrary, T2:
     Arbitrary]: Arbitrary[Notorious[O1,O2,A1,A2,T1,T2]] ={

          val genNotorious: Gen[Notorious[O1,O2,A1,A2,T1,T2]] = for {
               o1 <- Arbitrary.arbitrary[O1]
               o2 <- Arbitrary.arbitrary[O2]
               a1 <- Arbitrary.arbitrary[A1]
               a2 <- Arbitrary.arbitrary[A2]
               t1 <- Arbitrary.arbitrary[T1]
               t2 <- Arbitrary.arbitrary[T2]
          } yield Notorious(o1 => o2, a1 => a2, t1 => t2)

          Arbitrary(genNotorious)
     }
}
