package monoid.spec


import monoid.data._

import cats.Monoid
import cats.data.Validated.{Valid, Invalid}
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
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(true),
                    n = 5) shouldEqual ExclusiveDisjunction(true)
               // All true, even number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(true),
                    n = 4) shouldEqual ExclusiveDisjunction(false)

               //All false, even number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(false),
                    n = 5) shouldEqual ExclusiveDisjunction(false)
               //All False, odd number
               Monoid[ExclusiveDisjunction].combineN(ExclusiveDisjunction(false),
                    n = 4) shouldEqual ExclusiveDisjunction(false)




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

               //Even number of falses
               Monoid[ExclusiveNorDisjunction].combineAll(
                    List(ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false))) shouldEqual ExclusiveNorDisjunction(true)


               //Odd number of falses
               Monoid[ExclusiveNorDisjunction].combineAll(
                    List(ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(false),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(true),
                         ExclusiveNorDisjunction(false))) shouldEqual ExclusiveNorDisjunction(false)


               //TESTING: all same values, even or odd numbers

               //All false, even number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(false),
                    n = 5) shouldEqual ExclusiveNorDisjunction(false)
               //All False, odd number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(false),
                    n = 4) shouldEqual ExclusiveNorDisjunction(true)
               //All true, odd number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(true),
                    n = 5) shouldEqual ExclusiveNorDisjunction(true)
               // All true, even number
               Monoid[ExclusiveNorDisjunction].combineN(ExclusiveNorDisjunction(true),
                    n = 4) shouldEqual ExclusiveNorDisjunction(true)



               Monoid[ExclusiveNorDisjunction].empty shouldEqual ExclusiveNorDisjunction(true)
          }



          "-> AccumulateRight[String, Int] is a Monoid" in {

               Monoid[AccumulateRight[String, Int]].combineAll(List(

                    AccumulateRight(Invalid("Parse Error")),
                    AccumulateRight(Valid(12)),
                    AccumulateRight(Invalid("Config Error")),
                    AccumulateRight(Invalid("Key Error")),
                    AccumulateRight(Valid(23))

               )) shouldEqual AccumulateRight(Invalid("Parse Error"))

               Monoid[AccumulateRight[String, Int]].empty shouldEqual AccumulateRight(Valid(0))
          }


          //todo how to test this? Gives Comparison failure, no details on the function.
          /*"-> Memory holds a function of type S => (A, S), which is also a Monoid" in {

               val s: String = "reminder"

               Monoid[Memory[String, Int]].combineAll(List(

                    Memory(s => (s.length + 1, s + s.head.toUpper)),
                    Memory(s => (s.length, s))/*,
                    Memory((s: String) => (s.length *2, s + s))*/

               )) shouldEqual Memory((s:String) => (17, "reminderR"))
          }*/
     }
}
