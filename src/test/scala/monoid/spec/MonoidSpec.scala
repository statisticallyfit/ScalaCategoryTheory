package monoid.spec


import monoid.data._
import cats.Monoid
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import org.specs2.mutable._

/**
  *
  */

class MonoidSpec extends Specification {

     "Monoid is a typeclass that can be used to combine things of the same kind" should {

          "-> (Int, +) is a Monoid" in {

               Monoid[Int].combine(23, 17) shouldEqual 40
               Monoid[Int].combineN(2, 5) shouldEqual 10
               Monoid[Int].combineAll(Seq(1,2,3,1,8,9, 10)) shouldEqual 34
               Monoid[Int].isEmpty(4) shouldEqual false
               Monoid[Int].empty shouldEqual 0
          }

          "-> String is a Monoid" in {

               Monoid[String].combine("mountain", " cabin") shouldEqual "mountain cabin"
               Monoid[String].combineN("meow-", 3).dropRight(1) shouldEqual "meow-meow-meow"
               Monoid[String].combineAll(Seq("a","l","p","h","a","b","e","t")) shouldEqual "alphabet"
               Monoid[String].empty shouldEqual ""
          }



          "-> Option[Int] is a Monoid, where None is the empty value" in {

               Monoid[Option[Int]].combine(Some(1), Some(2)) shouldEqual Some(3)
               Monoid[Option[Int]].combineAll(List(None, Some(5), Some(7), None)) shouldEqual Some(12)
               Monoid[Option[Int]].empty shouldEqual None
          }


          "-> Two[Trivial, Option[Int]] is a Monoid" in {

               Monoid[Two[Trivial, Option[Int]]].combineAll(List(

                    Two(Trivial(), Some(1)), Two(Trivial(), None),
                    Two(Trivial(), Some(3)), Two(Trivial(), Some(5))

               )) shouldEqual Two(Trivial(), Some(9))

               Monoid[Two[Trivial, Option[Int]]].empty shouldEqual Two(Trivial(), None)
          }


          "-> Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction] is a Monoid" in {

               type Fiver = Five[Trivial, Conjunction, Disjunction, ExclusiveDisjunction, ExclusiveNorDisjunction]

               Monoid[Fiver].combineAll(
                    List(
                         Five(Trivial(), Conjunction(true), Disjunction(true), ExclusiveDisjunction(true),
                              ExclusiveNorDisjunction(false)),

                         Five(Trivial(), Conjunction(false), Disjunction(false), ExclusiveDisjunction(true),
                              ExclusiveNorDisjunction(false)),

                         Five(Trivial(), Conjunction(true), Disjunction(true), ExclusiveDisjunction(true),
                              ExclusiveNorDisjunction(false)),

                         Five(Trivial(), Conjunction(true), Disjunction(true), ExclusiveDisjunction(true),
                              ExclusiveNorDisjunction(false))
                    )
               ) shouldEqual Five(Trivial(), Conjunction(false), Disjunction(true), ExclusiveDisjunction(false),
                    ExclusiveNorDisjunction(true))


               Monoid[Fiver].empty shouldEqual Five(Trivial(),
                    Conjunction(true),
                    Disjunction(false),
                    ExclusiveDisjunction(false),
                    ExclusiveNorDisjunction(true))
          }


          "-> Boolean conjunction (and) is a Monoid where TRUE is the empty value" in {

               Monoid[Conjunction].combineAll(List(Conjunction(true), Conjunction(false),
                    Conjunction(true))) shouldEqual Conjunction(false)

               Monoid[Conjunction].combineAll(List(Conjunction(true), Conjunction(true),
                    Conjunction(true))) shouldEqual Conjunction(true)

               Monoid[Conjunction].combineN(Conjunction(true), 3) shouldEqual Conjunction(true)

               Monoid[Conjunction].empty shouldEqual Conjunction(true)
          }


          "-> Boolean disjunction (or) is a Monoid where FALSE is the empty value" in {

               Monoid[Disjunction].combineAll(List(Disjunction(true), Disjunction(false),
                    Disjunction(true))) shouldEqual Disjunction(true)

               Monoid[Disjunction].combineAll(List(Disjunction(false), Disjunction(false),
                    Disjunction(false))) shouldEqual Disjunction(false)

               Monoid[Disjunction].combineN(Disjunction(true), 3) shouldEqual Disjunction(true)

               Monoid[Disjunction].empty shouldEqual Disjunction(false)
          }


          "-> ExclusiveDisjunction is a Monoid where ODD number of trues is TRUE and EVEN number of trues is " +
               "FALSE" in {

               //Testing two at a time (truth table)

               Monoid[ExclusiveDisjunction].combine(
                    ExclusiveDisjunction(true),
                    ExclusiveDisjunction(true)) shouldEqual ExclusiveDisjunction(false)

               Monoid[ExclusiveDisjunction].combine(
                    ExclusiveDisjunction(true),
                    ExclusiveDisjunction(false)) shouldEqual ExclusiveDisjunction(true)

               Monoid[ExclusiveDisjunction].combine(
                    ExclusiveDisjunction(false),
                    ExclusiveDisjunction(false)) shouldEqual ExclusiveDisjunction(false)


               //TESTING: more values at once

               //Even TRUES, Even FALSE
               Monoid[ExclusiveDisjunction].combineAll(
                    List(ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false))) shouldEqual ExclusiveDisjunction(false)

               //Even TRUES, Odd FALSE
               Monoid[ExclusiveDisjunction].combineAll(
                    List(ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(false))) shouldEqual ExclusiveDisjunction(false)

               //Odd TRUES, Odd False
               Monoid[ExclusiveDisjunction].combineAll(
                    List(ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false))) shouldEqual ExclusiveDisjunction(true)

               //Odd TRUES, Even False
               Monoid[ExclusiveDisjunction].combineAll(
                    List(ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(true),
                         ExclusiveDisjunction(false),
                         ExclusiveDisjunction(false))) shouldEqual ExclusiveDisjunction(true)


               //TESTING: all same values, even or odd numbers

               //All true, odd number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(true), n = 5) shouldEqual
                    ExclusiveDisjunction(true)
               // All true, even number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(true),  n = 4) shouldEqual
                    ExclusiveDisjunction(false)

               //All false, even number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(false), n = 5) shouldEqual
                    ExclusiveDisjunction(false)
               //All False, odd number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(false), n = 4) shouldEqual
                    ExclusiveDisjunction(false)




               Monoid[ExclusiveDisjunction].empty shouldEqual ExclusiveDisjunction(false)
          }


          "-> ExclusiveNorDisjunction is a Monoid where EVEN number of falses results in TRUE and ODD number of " +
               "falses results in FALSE" in {

               //Testing two at a time (truth table)

               Monoid[ExclusiveNorDisjunction].combine(
                    ExclusiveNorDisjunction(true),
                    ExclusiveNorDisjunction(true)) shouldEqual ExclusiveNorDisjunction(true)

               Monoid[ExclusiveNorDisjunction].combine(
                    ExclusiveNorDisjunction(true),
                    ExclusiveNorDisjunction(false)) shouldEqual ExclusiveNorDisjunction(false)

               Monoid[ExclusiveNorDisjunction].combine(
                    ExclusiveNorDisjunction(false),
                    ExclusiveNorDisjunction(false)) shouldEqual ExclusiveNorDisjunction(true)




               //TESTING: more values at once


               //Even TRUES, Even FALSE
               Monoid[ExclusiveNorDisjunction].combineAll(
                    List(ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false))) shouldEqual ExclusiveNorDisjunction(true)

               //Even TRUES, Odd FALSE
               Monoid[ExclusiveNorDisjunction].combineAll(
                    List(ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(false))) shouldEqual ExclusiveNorDisjunction(false)

               //Odd TRUES, Odd False
               Monoid[ExclusiveNorDisjunction].combineAll(
                    List(ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false))) shouldEqual ExclusiveNorDisjunction(false)

               //Odd TRUES, Even False
               Monoid[ExclusiveNorDisjunction].combineAll(
                    List(ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(false))) shouldEqual ExclusiveNorDisjunction(true)


               //TESTING: all same values, even or odd numbers

               //All false, even number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(false), n = 5) shouldEqual
                    ExclusiveNorDisjunction(false)
               //All False, odd number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(false), n = 4) shouldEqual
                    ExclusiveNorDisjunction(true)
               //All true, odd number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(true), n = 5) shouldEqual
                    ExclusiveNorDisjunction(true)
               // All true, even number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(true), n = 4) shouldEqual
                    ExclusiveNorDisjunction(true)



               Monoid[ExclusiveNorDisjunction].empty shouldEqual ExclusiveNorDisjunction(true)
          }

          "-> Validated[String, Int] is a Monoid" in {

               Monoid[Validated[String, Int]].combineAll(List(
                    Invalid("Error 1"),
                    Valid(4567),
                    Invalid("Error 2"),
                    Valid(12)
               )) shouldEqual Invalid("Error 1Error 2")

               Monoid[Validated[String, Int]].empty shouldEqual Valid(0)

               Monoid[Validated[String, String]].empty shouldEqual Valid("")

               Monoid[Validated[String, Conjunction]].empty shouldEqual Valid(Monoid[Conjunction].empty)
          }


          "-> MyValidated[String, Int] is a Monoid" in {

               Monoid[MyValidated[String, Int]].combineAll(List(
                    NotValid("Error 1"),
                    IsValid(4567),
                    NotValid("Error 2"),
                    IsValid(12)
               )) shouldEqual NotValid("Error 1Error 2")


               Monoid[MyValidated[String, Int]].empty shouldEqual IsValid(0)
               Monoid[MyValidated[String, String]].empty shouldEqual IsValid("")

               Monoid[MyValidated[String, Conjunction]].empty shouldEqual IsValid(Monoid[Conjunction].empty)
          }



          "-> AccumulateRight[String, Int] is a Monoid" in {


               Monoid[AccumulateRight[String, Int]].combine(AccumulateRight(Invalid("Err1")),
                    AccumulateRight(Invalid("Err2"))) shouldEqual AccumulateRight(Invalid("Err2"))


               Monoid[AccumulateRight[String, Int]].combineAll(List(

                    AccumulateRight(Invalid("Parse Error")),
                    AccumulateRight(Valid(12)),
                    AccumulateRight(Invalid("Config Error")),
                    AccumulateRight(Invalid("Key Error")),
                    AccumulateRight(Valid(23))

               )) shouldEqual AccumulateRight(Invalid("Key Error"))

               Monoid[AccumulateRight[String, Int]].empty shouldEqual AccumulateRight(Valid(0))


               Monoid[AccumulateRight[String, Disjunction]].empty shouldEqual
                    AccumulateRight(Valid(Monoid[Disjunction].empty)) //Disjunction(false)
          }

          "-> AccumulateBoth[String, Int] is a Monoid" in {

               Monoid[AccumulateBoth[Disjunction, Conjunction]].combineAll(List(
                    AccumulateBoth(Invalid(Disjunction(false))),
                    AccumulateBoth(Invalid(Disjunction(true)))

               )) shouldEqual AccumulateBoth(Invalid(Disjunction(true)))


               Monoid[AccumulateBoth[Disjunction, Conjunction]].combineAll(List(
                    AccumulateBoth(Valid(Conjunction(false))),
                    AccumulateBoth(Valid(Conjunction(true)))

               )) shouldEqual AccumulateBoth(Valid(Conjunction(false)))



               Monoid[AccumulateBoth[Disjunction, Conjunction]].combineAll(List(
                    AccumulateBoth(Valid(Conjunction(false))),
                    AccumulateBoth(Valid(Conjunction(true))),
                    AccumulateBoth(Invalid(Disjunction(true))),
                    AccumulateBoth(Valid(Conjunction(true)))

               )) shouldEqual AccumulateBoth(Invalid(Disjunction(true)))


               Monoid[AccumulateBoth[String, Int]].combineAll(List(

                    AccumulateBoth(Invalid("Parse Error")),
                    AccumulateBoth(Valid(12)),
                    AccumulateBoth(Invalid("Config Error")),
                    AccumulateBoth(Invalid("Key Error")),
                    AccumulateBoth(Valid(23))

               )) shouldEqual AccumulateBoth(Invalid("Parse ErrorConfig ErrorKey Error"))


               Monoid[AccumulateBoth[String, Int]].empty shouldEqual AccumulateBoth(Valid(0))

               Monoid[AccumulateBoth[String, Disjunction]].empty shouldEqual
                    AccumulateBoth(Valid(Disjunction(false)))

               Monoid[AccumulateBoth[String, Two[Conjunction, Disjunction]]].empty shouldEqual
                    AccumulateBoth(Valid(Two(Conjunction(true), Disjunction(false))))
          }


          "-> MyFunction[Two[Int, Int], Int] is a Monoid" in {
               val alpha: Two[Int, Int] => String = (two: Two[Int, Int]) => s"${two.a} + ${two.b}"
               val beta: Two[Int, Int] => String = (two: Two[Int, Int]) => s"${two.a} * ${two.b}"

               val f: Two[Int, Int] => Int = (two: Two[Int, Int]) => two.a + two.b
               val g: Two[Int, Int] => Int = (two: Two[Int, Int]) => two.a * two.b


               alpha(Two(4,2)) shouldEqual "4 + 2"
               beta(Two(5, 3)) shouldEqual "5 * 3"

               f(Two(8, 9)) shouldEqual 17
               g(Two(3, 7)) shouldEqual 21


               val strChecker: Two[Int,Int] => String =
                    (two: Two[Int,Int]) => Monoid[String].combine(alpha(two), beta(two))


               val intChecker: Two[Int,Int] => Int =
                    (two: Two[Int,Int]) => Monoid[Int].combine(f(two), g(two))


               // Method 1 ---------------------------------------------------------------------------------------
               val combineTwoStr: MyFunction[Two[Int, Int], String] = Monoid[MyFunction[Two[Int, Int], String]].combine(
                    MyFunction(alpha), MyFunction(beta)
               )

               combineTwoStr.inner(Two(4,2)) shouldEqual "4 + 24 * 2" //combines the end results of each function
               strChecker(Two(4,2)) shouldEqual "4 + 24 * 2"

               //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

               val combineTwoInt: MyFunction[Two[Int, Int], Int] = Monoid[MyFunction[Two[Int,Int], Int]].combine(
                    MyFunction(f), MyFunction(g)
               )

               combineTwoInt.inner(Two(8, 3)) shouldEqual 35
               intChecker(Two(8, 3)) shouldEqual 35


               // Method 2 ---------------------------------------------------------------------------------------

               val combinedStr: MyFunction[Two[Int, Int], String] = MyFunction(alpha).combine(MyFunction(beta)) //(Two
               // (4,2))

               combinedStr.inner(Two(4,2)) shouldEqual "4 + 24 * 2"
               strChecker(Two(4,2)) shouldEqual "4 + 24 * 2"

               //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

               val combinedInt: MyFunction[Two[Int, Int], Int] = MyFunction(f).combine(MyFunction(g))

               combinedInt.inner(Two(4, 2)) shouldEqual 14
               intChecker(Two(4, 2)) shouldEqual 14


               // Method 3 ---------------------------------------------------------------------------------------
               val combineInnerStr: Two[Int, Int] => String = Monoid[Two[Int,Int] => String].combine(alpha, beta)

               combineInnerStr(Two(4,2)) shouldEqual "4 + 24 * 2"

               //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
               val combineInnerInt: Two[Int, Int] => Int = Monoid[Two[Int, Int] => Int].combine(f, g)

               combineInnerInt(Two(4, 2)) shouldEqual 14
               intChecker(Two(4, 2)) shouldEqual 14



               // Empty tests ----------------
               Monoid[Two[Int, Int] => Validated[String, Int]].empty(Two(1,2)) shouldEqual Valid(0)
               Monoid[Two[Int, Int] => Validated[Int, Conjunction]].empty(Two(1,2)) shouldEqual Valid(Conjunction(true))


          }


          "-> Memory[String, Int] is a Monoid" in {

               val input: String = "mermaid"
               var LEN = input.length

               val g: String => (Int, String) = (str: String) => (LEN + 2, s"g($str)")
               val f: String => (Int, String) = (str: String) => (LEN * 4, s"f($str)")


               val result: Memory[String, Int] = Monoid[Memory[String, Int]].combine(Memory(f), Memory(g))

               val (lenPlusTwo, inG) = g(input)
               val (lenTimesFour, inF) = f(inG)

               result.runMem(input) shouldEqual (lenPlusTwo + lenTimesFour, inF)
               result.runMem(input) shouldEqual (Monoid[Int].combine(lenPlusTwo, lenTimesFour), s"f(g($input))")
               result.runMem(input) shouldEqual (LEN + 2 + LEN*4, s"f(g($input))")

               //-------------------------------------------------------------------------------------------

               val res = Monoid[Memory[String, Int]].combineAll(
                    List(
                         Memory((s:String) => (s.length + 2, s.head.toUpper.toString + s.tail)),
                         Memory((s:String) => (s.length * 4, s.head.toLower.toString + s.tail.head.toUpper.toString + s.tail
                              .tail)),
                         Memory((s:String) => (s.length + 5, s.head.toString + s.tail.head.toLower.toString + s.tail.tail.head.toUpper.toString +
                              s.tail.tail.tail))
                    ).reverse
               )


               res.runMem("quicksilver") shouldEqual (73, "quIcksilver")


               //val s = "quicksilver"
               LEN = "quicksilver".length

               val memoryForwards: Memory[String, Int] = Monoid[Memory[String, Int]].combineAll(List(
                    Memory((s:String) => (LEN + 2, s"(2 + ${s.length})")),
                    Memory((s:String) => (LEN * 4, s"(4 * $s)")),
                    Memory((s:String) => (LEN + 5, s"(5 + $s)"))
               ).reverse)

               memoryForwards.runMem("quicksilver") shouldEqual (Monoid[Int].combineAll(List(LEN + 2, LEN * 4, LEN + 5)), "(5 + (4 * (2 + 11)))")


               val memoryBackwards: Memory[String, Int] = Monoid[Memory[String, Int]].combineAll(List(
                    Memory((s:String) => (LEN + 2, s"(2 + $s)")),
                    Memory((s:String) => (LEN * 4, s"(4 * $s)")),
                    Memory((s:String) => (LEN + 5, s"(5 + ${s.length})"))
               ))
               memoryBackwards.runMem("quicksilver") shouldEqual (Monoid[Int].combineAll(List(LEN + 2, LEN * 4, LEN + 5)), "(2 + (4 * (5 + 11)))")
          }
     }
}
