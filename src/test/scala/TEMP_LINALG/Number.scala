package TEMP_LINALG


import TEMP_LINALG.theory._
import TEMP_LINALG.syntax.AbsoluteSyntax._
import TEMP_LINALG.syntax.EqualSyntax._
import TEMP_LINALG.syntax.NumberSyntax._
import TEMP_LINALG.syntax.RootSyntax._
import TEMP_LINALG.syntax.ShowSyntax._
import TEMP_LINALG.syntax.TrigSyntax._


import org.apache.commons.lang3.math.Fraction

import scala.language.implicitConversions


/**
  * A basic Number system that supports trigonometry, equality and different number types: Complex, Real, Rational,
  * Int, Double.
  * Functionality is implemented using a typeclass pattern to preserve cleanliness and maintainability.
  *
  *
  * Features:
  * - rational number reducability upon creation
  * - complex number .i creation
  * - complex numbers can have rational arguments too, anything that implements the RealNumber typeclass.
  * - printing occurs via neat Show trait, just like in Haskell.
  * - interoperability between Complex[R] and R types.
  * - trigonometric Real and Double types
  * - complex roots of unity and nth roots functions.
  *  - Trig implementations - making RealLike extend Trig so when we declare Reals we must declare Trigs too.
  *
  *
  * note: Source for complex .i accessor:
  * https://stackoverflow.com/questions/17381896/scala-simple-notation-of-imaginary-number
  */
//TODO: Number: make self-type this: Absolute[Number[N], N] ... etc - if it works?
// TODO so that RealLike inherits - but must  also include: this: Trig[R] =>

// TODO   must change Conversion trait so that we have Number[N, R] and RealLike[R] extends
// TODO Number[R,R] and we have Number[Complex[R],R] instance and the rest are RealLike-s.
// TODO Then in the Number trait we add conversion methods: def plus(const: R, num: N): N
// TODO and def plus(num: N, const: R): N and good old def plus(n1: N, n2: N): N  and
// TODO so on for each operation. Then maybe this will guarantee type conversion? Like
// TODO Real + complex => complex ... so no need for the hardcoded-complex-int conversion,
// TODO we have interoperability automatically.

trait Number[N] extends Field[N]  {

     val zero: N
     val one: N
     val two: N

     def plus(x: N, y: N): N
     def minus(x: N, y: N): N = plus(x, negate(y))
     def times(x: N, y: N): N
     def divide(x: N, y: N): N
     def negate(x: N): N
     def inverse(x: N): N
     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean

     def doubleValue(x: N): Double
     def from(x: Int): N
}

trait Equal[E]  {
     def equal(x: E, y: E): Boolean
     def lessThan(x: E, y: E): Boolean
     def greaterThan(x: E, y: E): Boolean = lessThan(y, x)
     def lessThanOrEqual(x: E, y: E): Boolean = lessThan(x, y) || equal(x, y)
     def greaterThanOrEqual(x: E, y: E): Boolean = greaterThan(x, y) || equal(x, y)
}

trait Absolute[H, L]{
     def absoluteValue(x: H): L
}

trait Root[H, L]{
     def power(base: H, exp: L): H
     def nRoot(base: H, n: L): H // = power(base, one / n)
     def squareRoot(base: H): H // = nRoot(base, two)
}

trait Trig[T] {

     val E: T
     val PI: T

     def sin(x: T): T
     def cos(x: T): T
     def tan(x: T): T
     def csc(x: T): T
     def sec(x: T): T
     def cot(x: T): T

     def arcsin(x: T): T
     def arccos(x: T): T
     def arctan(x: T): T
     def arccsc(x: T): T
     def arcsec(x: T): T
     def arccot(x: T): T

     //returns the theta component of polar (r, theta) of the x-y coordinate (x: T, y: T)
     def theta(y: T, x: T): T
}



trait RealLike[R] extends Number[R] {

     def from(x: Int): R
}





object Number {

     def ZERO[N](implicit gen: Number[N]): N = gen.zero
     def ONE[N](implicit gen: Number[N]): N = gen.one
     def TWO[N](implicit gen: Number[N]): N = gen.two



     implicit def ComplexIsNumber[R: RealLike: Equal: Trig](implicit rr: Root[R,R],
                                                            pos: Absolute[R,R]) = new Number[Complex[R]]
          with Equal[Complex[R]] with Root[Complex[R], R] with Absolute[Complex[R], R] {



          type C = Complex[R]
          val realLike = implicitly[RealLike[R]]


          /** Number part */
          val zero: C = Complex.ZERO[R]
          val one: C = Complex.ONE[R]
          val two: C = Complex.TWO[R]

          def plus(x: C, y: C): C = Complex(x.re + y.re, x.im + y.im)
          def times(x: C, y: C): C = Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
          def divide(x: C, y: C): C = {
               val prod: C = times(x, y)
               val absDenom: R = Complex.magnitude(y)
               Complex(prod.re / absDenom, prod.im / absDenom)
          }
          def negate(x: C): C = Complex(x.re.negate(), x.im.negate())
          def isZero(x: C): Boolean = equal(x, zero)
          def isNegative(x: C): Boolean = x.re.isNegative && x.im.isNegative
          def isReal(x: C): Boolean = x.im.isZero //todo make this visible in complex class or object.
          def isImaginary(x: C): Boolean = !isReal(x)
          def doubleValue(x: C): Double = Complex.magnitude(x).toDouble
          def from(x: Int): C = Complex(realLike.from(x))

          /** Equality part */
          //def eqv(x: C, y: C): Boolean = Eq[R].eqv(x.re, y.re) && Eq[R].eqv(x.im, y.im)
          def equal(x: C, y: C): Boolean = x.re :==: y.re && x.im :==: y.im
          def lessThan(x: C, y: C): Boolean = x.re < y.re || (x.re :==: y.re && x.im < y.im)


          /** Root part */
          def power(base: Complex[R], exp: R): Complex[R] =
               Complex(rr.power(Complex.magnitude(base), exp), Complex.angle(base) * exp)

          def nRoot(base: Complex[R], n: R): Complex[R] = {
               val (modulusRoot, listRoots): (R, List[R]) = Complex.nthRootComplex(base, n)
               Complex(modulusRoot, listRoots.head) //just returning first root for convenience.
          }

          def squareRoot(base: Complex[R]): Complex[R] = nRoot(base, realLike.one / realLike.two)


          /** Absolute part */
          def absoluteValue(z: Complex[R]): R = Complex.magnitude(z)
     }



     implicit object RealIsNumber extends RealLike[Real] with Equal[Real] with Absolute[Real, Real]
          with Root[Real, Real] with Trig[Real] {

          /** Real part */
          val zero: Real = Real.ZERO
          val one: Real = Real.ONE
          val two: Real = Real.TWO

          def plus(x: Real, y: Real): Real = Real(x.double + y.double)
          def times(x: Real, y: Real): Real = Real(x.double * y.double)
          def divide(x: Real, y: Real): Real = Real(x.double / y.double)
          def power(base: Real, exp: Real): Real = Real(math.pow(base.double, exp.double))
          def nRoot(base: Real, n: Real): Real = power(base, divide(one, n))
          def squareRoot(base: Real): Real = nRoot(base, two)
          def absoluteValue(x: Real): Real = Real(math.abs(x.double))
          def negate(x: Real): Real = Real(-x.double)
          def isZero(x: Real): Boolean = equal(x, zero)
          def isNegative(x: Real): Boolean = x.double < 0
          def doubleValue(x: Real): Double = x.double
          def from(x: Int): Real = Real(x)


          /** Equality part */
          def equal(x: Real, y: Real): Boolean = x.double == y.double
          def lessThan(x: Real, y: Real): Boolean = x.double < y.double


          /** Trig part **/
          val E: Real = Real(scala.math.E)
          val PI: Real = Real(scala.math.Pi)

          def sin(x: Real): Real = Real(math.sin(x.double))
          def cos(x: Real): Real = Real(math.cos(x.double))
          def tan(x: Real): Real = Real(math.tan(x.double))
          def csc(x: Real): Real = divide(Real.ONE, sin(x))
          def sec(x: Real): Real = divide(Real.ONE, cos(x))
          def cot(x: Real): Real = divide(Real.ONE, tan(x))

          def arcsin(x: Real): Real = Real(math.asin(x.double))
          def arccos(x: Real): Real = Real(math.acos(x.double))
          def arctan(x: Real): Real = Real(math.atan(x.double))
          def arccsc(x: Real): Real = divide(Real.ONE, arcsin(x))
          def arcsec(x: Real): Real = divide(Real.ONE, arccos(x))
          def arccot(x: Real): Real = divide(Real.ONE, arctan(x))

          def theta(y: Real, x: Real): Real = Real(math.tan(y.double / x.double))
     }


     implicit object RationalIsRealNumber extends RealLike[Rational] with Equal[Rational] with Absolute[Rational, Rational]
          with Root[Rational, Rational] with Trig[Rational] {

          /** Real part */
          val zero: Rational = Rational.ONE
          val one: Rational = Rational.ONE
          val two: Rational = Rational.TWO

          def plus(x: Rational, y: Rational): Rational = Rational(x.num*y.den + y.num*x.den, x.den*y.den)
          def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
          def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)
          def power(base: Rational, exp: Rational): Rational = Rational(math.pow(doubleValue(base), doubleValue(exp)))
          def nRoot(base: Rational, n: Rational): Rational = power(base, divide(one, n))
          def squareRoot(base: Rational): Rational = nRoot(base, two)
          def absoluteValue(x: Rational): Rational = Rational(math.abs(x.num), math.abs(x.den))
          def negate(x: Rational): Rational = Rational(-x.num, -x.den)
          def areEqual(x: Rational, y: Rational): Boolean = x.num == y.num && x.den == y.den
          def isZero(x: Rational): Boolean = areEqual(x, zero)
          def isNegative(x: Rational): Boolean = x.num < 0
          def doubleValue(x: Rational): Double = x.num * 1.0 / x.den
          def from(x: Int): Rational = Rational(x)


          /** Equality part **/
          def equal(x: Rational, y: Rational): Boolean = x.num * y.den == y.num * x.den
          def lessThan(x: Rational, y: Rational): Boolean = x.num * y.den < y.num * x.den


          /** Trig part **/
          val E: Rational = Rational(scala.math.E)
          val PI: Rational = Rational(scala.math.Pi) // this is what spire does too, because these are finit     e.

          def sin(x: Rational): Rational = Rational(math.sin(doubleValue(x)))
          def cos(x: Rational): Rational = Rational(math.cos(doubleValue(x)))
          def tan(x: Rational): Rational = Rational(math.tan(doubleValue(x)))
          def csc(x: Rational): Rational = divide(Rational.ONE, sin(x))
          def sec(x: Rational): Rational = divide(Rational.ONE, cos(x))
          def cot(x: Rational): Rational = divide(Rational.ONE, tan(x))

          def arcsin(x: Rational): Rational = Rational(math.asin(doubleValue(x)))
          def arccos(x: Rational): Rational = Rational(math.acos(doubleValue(x)))
          def arctan(x: Rational): Rational = Rational(math.atan(doubleValue(x)))
          def arccsc(x: Rational): Rational = divide(Rational.ONE, arcsin(x))
          def arcsec(x: Rational): Rational = divide(Rational.ONE, arccos(x))
          def arccot(x: Rational): Rational = divide(Rational.ONE, arctan(x))

          def theta(y: Rational, x: Rational): Rational = Rational(math.tan(doubleValue(x) / doubleValue(x)))
     }


     implicit object IntIsRealNumber extends RealLike[Int] with Equal[Int] with Absolute[Int, Int]
          with Root[Int, Int] with Trig[Int] {

          /** Real part **/
          val one: Int = 1
          val zero: Int = 0
          val two: Int = 2

          def plus(x: Int, y: Int): Int = x + y
          def times(x: Int, y: Int): Int = x * y
          def divide(x: Int, y: Int): Int = x / y
          def power(base: Int, exp: Int): Int = math.pow(base, exp).toInt //not chopped off
          def nRoot(base: Int, n: Int): Int = math.pow(base, 1.0 / n).toInt //todo gets chopped off
          def squareRoot(base: Int): Int = nRoot(base, two)
          def absoluteValue(x: Int): Int = math.abs(x)
          def negate(x: Int): Int = -x
          def isZero(x: Int): Boolean = x == 0
          def isNegative(x: Int): Boolean = x < 0
          def areEqual(x: Int, y: Int): Boolean = x == y
          def doubleValue(x: Int): Double = x * 1.0
          def from(x: Int): Int = x


          /** Equality part **/
          def equal(x: Int, y: Int): Boolean = x == y
          def lessThan(x: Int, y: Int): Boolean = x < y


          /** Trig part **/
          val E: Int = 2
          val PI: Int = 3 //just approximations! - note: int is not good for calculations

          def sin(x: Int): Int = math.sin(x).toInt
          def cos(x: Int): Int = math.cos(x).toInt
          def tan(x: Int): Int = math.tan(x).toInt
          def csc(x: Int): Int = (1.0 / sin(x)).toInt
          def sec(x: Int): Int = (1.0 / cos(x)).toInt
          def cot(x: Int): Int = (1.0 / tan(x)).toInt

          def arcsin(x: Int): Int = math.asin(x).toInt
          def arccos(x: Int): Int = math.acos(x).toInt
          def arctan(x: Int): Int = math.atan(x).toInt
          def arccsc(x: Int): Int = (1.0 / arcsin(x)).toInt
          def arcsec(x: Int): Int = (1.0 / arccos(x)).toInt
          def arccot(x: Int): Int = (1.0 / arctan(x)).toInt

          def theta(y: Int, x: Int): Int = math.tan(y / x).toInt
     }


     implicit object DoubleIsRealNumber extends RealLike[Double] with Equal[Double] with Absolute[Double, Double]
          with Root[Double, Double] with Trig[Double] {

          /** Real part **/
          val one: Double = 1.0
          val zero: Double = 0.0
          val two: Double = 2.0

          def plus(x: Double, y: Double): Double = x + y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y
          def power(base: Double, exp: Double): Double = math.pow(base, exp)
          def squareRoot(base: Double): Double = math.sqrt(base)
          def nRoot(base: Double, n: Double): Double = power(base, 1/n)
          def absoluteValue(x: Double): Double = math.abs(x)
          def negate(x: Double): Double = -x
          def isZero(x: Double): Boolean = x == 0
          def isNegative(x: Double): Boolean = x < 0
          def areEqual(x: Double, y: Double): Boolean = x == y
          def doubleValue(x: Double): Double = x
          def from(x: Int): Double = x * 1.0


          /** Equality part **/
          def equal(x: Double, y: Double): Boolean = x == y
          def lessThan(x: Double, y: Double): Boolean = x < y


          /** Trig part **/
          val E: Double = scala.math.E
          val PI: Double = scala.math.Pi

          def sin(x: Double): Double = math.sin(x)
          def cos(x: Double): Double = math.cos(x)
          def tan(x: Double): Double = math.tan(x)
          def csc(x: Double): Double = 1.0 / sin(x)
          def sec(x: Double): Double = 1.0 / cos(x)
          def cot(x: Double): Double = 1.0 / tan(x)

          def arcsin(x: Double): Double = math.asin(x)
          def arccos(x: Double): Double = math.acos(x)
          def arctan(x: Double): Double = math.atan(x)
          def arccsc(x: Double): Double = 1.0 / arcsin(x)
          def arcsec(x: Double): Double = 1.0 / arccos(x)
          def arccot(x: Double): Double = 1.0 / arctan(x)

          def theta(y: Double, x: Double): Double = math.tan(y / x)
     }
}



// ---------------------------------------------------------------------------------------------------------



//represents a conversion between numbers
//todo is it weird that it has similar methods as Number? Repetitive? Maybe have general number type with
// SimpleNumber[R] ext Number[R, R]
trait Conversion[F, T] {
     def plus(from: F, to: T): T
     def minus(from: F, to: T): T
     def times(from: F, to: T): T
     def divide(from: F, to: T): T
     def power(base: T, exp: F): T
}
object Conversion {
     //mechanism: takes something that implements RealNumber and gives it .i accessor, returning Imaginary.
     implicit class ToImaginary[R: RealLike](private val imaginaryPart: R){

          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }

     //mechanism: takes something that implements RealNumber and makes it addable with Imaginary (which BTW cannot
     // implement Number because i*i = -1, not imaginary)
     implicit class ToComplex[R: RealLike](private val realPart: R)/*(implicit compLike: ComplexLike[Imaginary[R], R])*/ {

          def +(that: Imaginary[R]) = Complex(realPart, that.im) //compLike.imag(that)) //can just do that.im
          def -(that: Imaginary[R]) = Complex(realPart, that.im.negate()) //compLike.imag(that).negate())
     }

     // ---------------------------------------------------------------------------------------------

     implicit def GeneralRealToComplex[R: RealLike](implicit rt: Root[Complex[R], R]) =
          new Conversion[R, Complex[R]]{

               def plus(from: R, to: Complex[R]): Complex[R] = Complex(from + to.re, to.im)
               def minus(from: R, to: Complex[R]): Complex[R] = Complex(from - to.re, to.im)
               def times(from: R, to: Complex[R]): Complex[R] = Complex(from * to.re, from * to.im)
               def divide(from: R, to: Complex[R]): Complex[R] = Complex(to.re / from, to.im / from)
               def power(base: Complex[R], exp: R): Complex[R] = base ^ exp
          }

     implicit class ConvertFrom[F, T](val from: F)(implicit conv: Conversion[F, T]){
          def +(to: T): T = conv.plus(from, to)
          def -(to: T): T = conv.minus(from, to)
          def *(to: T): T = conv.times(from, to)
          def /(to: T): T = conv.divide(from, to)
          def ^(exp: T): T = conv.power(exp, from)
     }
     implicit class ConvertTo[F, T](val to: T)(implicit conv: Conversion[F, T]){
          def +(from: F): T = conv.plus(from, to)
          def -(from: F): T = conv.minus(from, to)
          def *(from: F): T = conv.times(from, to)
          def /(from: F): T = conv.divide(from, to)
          def ^(exp: F): T = conv.power(to, exp)
     }
}
import Conversion._




// ---------------------------------------------------------------------------------------------------------


case class Real(double: Double) {
     override def toString = Real(double).show
}


case class Rational(private val n: Int, private val d: Int) {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     override def toString: String = Rational(num, den).show
}


case class Complex[R:RealLike](re:R, im:R) {
     override def toString: String = Complex(re, im).show
}


case class Imaginary[R: RealLike](im: R) {

     implicit def i: Imaginary[R] = this

     override def toString: String = im match {
          case _: Rational => im.isNegative match {
               case true => " - (" + im.negate().toString + ")" + "i"
               case false => " + (" + im.toString + ")" + "i"
          }
          case _ => im.isNegative match {
               case true => " - " + im.negate().toString + "i"
               case false => " + " + im.toString + "i"
          }
     }
}





object Complex {

     def ZERO[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.one, gen.zero)
     def TWO[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.two, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealLike[R]): Complex[R] = new Complex(realPart, gen.zero)


     // --- Operations ---
     def polar[R: RealLike](z: Complex[R])(implicit rr: Root[R,R], t: Trig[R]): Complex[R] =
          Complex(magnitude(z), angle(z))

     def magnitude[R: RealLike](z: Complex[R])(implicit rr: Root[R,R]): R =
          (z.re * z.re + z.im * z.im).sqrt()

     //just returns the value of theta for the complex number: theta = arctan(b / a), where c = a + bi
     def angle[R: RealLike](z: Complex[R])(implicit trig: Trig[R]): R = trig.theta(z.re, z.im)

     /** Returns the nth root of a complex number - in tuple form = (modulus root n, list of all roots) */
     def nthRootComplex[R](z: Complex[R], n: R)(implicit gen: RealLike[R],
                                                trig: Trig[R],
                                                rr: Root[R,R]): (R, List[R]) ={

          val two: R = gen.one + gen.one
          val polarComplex: Complex[R] = polar(z)
          val (modulus, theta): (R, R) = (polarComplex.re, polarComplex.im)

          val theNRoots: List[R] = List.tabulate[R](n.toInt)(k => (theta + two * gen.from(k) * trig.PI) / n)

          (modulus.nRoot(n), theNRoots)
     }

     def nthRootsOfUnity[R](z: Complex[R], n: R)(implicit gen: RealLike[R], trig: Trig[R]): List[R] = {
          val two: R = gen.one + gen.one
          List.tabulate[R](n.toInt)(k => (two * gen.from(k) * trig.PI) / n)
     }

     def conjugate[R: RealLike](z: Complex[R]): Complex[R] = Complex(z.re, z.im.negate())
}


object Imaginary {

     def ZERO[R](implicit gen: RealLike[R]): Imaginary[R] = new Imaginary(gen.zero)
     def ONE[R](implicit gen: RealLike[R]): Imaginary[R] = new Imaginary(gen.one)
     def TWO[R](implicit gen: RealLike[R]): Imaginary[R] = new Imaginary(gen.two)
}


object Real {
     val ZERO: Real = new Real(0)
     val ONE: Real = new Real(1)
     val TWO: Real = new Real(2)
}

object Rational {
     val ZERO: Rational = new Rational(0, 1)
     val ONE: Rational = new Rational(1, 1)
     val TWO: Rational = new Rational(2, 1)

     def apply(numerator: Int): Rational = new Rational(numerator, 1)

     def apply(fractionAsDouble: Double): Rational = {
          val f = Fraction.getFraction(fractionAsDouble).reduce()
          new Rational(f.getNumerator, f.getDenominator)
     }
}


// ---------------------------------------------------------------------------------------------------------



object NumberTester extends App {


     import Number._

     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i + Rational(1)
     val b: Complex[Int] = 3 + 5.i + 3
     val c: Complex[Int] = 1 - 2.i

     val r1: Rational = Rational(2)
     val r2: Rational = Rational(4,5)

     //println(a.testing(Rational(2)))

     println(r1 + r2)
     println(c)
     println(b < c)
     println(b :==: c)
     println((4 + 3.i) :==: (4 + 3.i))
     println((2 + 5.i) < (2 + 7.i))
     println((2 + 5.i) < (2 - 5.i))

     println(a)
     println(b)
     println(a + Rational(1))
     println(Rational(33) + a)
     println(23.0 + (1.0 + 3.0.i))
     println((1.5 + 3.2.i) + 23.2)
     println((1 + 3.i) + 1)
     println(1 + (1 + 3.i))
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     println((8 + 2.i) < (9 + 2.i))

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))
}