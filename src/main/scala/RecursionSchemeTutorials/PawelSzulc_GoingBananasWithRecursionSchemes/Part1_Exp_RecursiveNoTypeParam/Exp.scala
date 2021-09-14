package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.Part1_Exp_RecursiveNoTypeParam

/**
 *
 */
sealed trait Exp
final case class IntValue(v: Int) extends Exp
final case class DecValue(v: Double) extends Exp
final case class Sum(exp1: Exp, exp2: Exp) extends Exp
final case class Multiply(exp1: Exp, exp2: Exp) extends Exp
final case class Divide(exp1: Exp, exp2: Exp) extends Exp
final case class Square(exp: Exp) extends Exp

object Exp {

	val evaluate: Exp => Double = exp => exp match {
		case IntValue(v) => v.toDouble
		case DecValue(v) => v
		case Sum(e1, e2) => evaluate(e1) + evaluate(e2) // recursion part
		case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)
		case Square(e) => {
			val v = evaluate(e)
			v * v
		}
		case Divide(e1, e2) => evaluate(e1) / evaluate(e2)
	}

	val mkString: Exp => String = exp => exp match {
		case IntValue(v) => v.toString
		case DecValue(v) => v.toString
		case Sum(e1, e2) => s"(${mkString(e1)} + ${mkString(e2)})"
		case Multiply(e1, e2) => s"(${mkString(e1)} * ${mkString(e2)})"
		case Square(e) => s"(${mkString(e)}^2)"
		case Divide(e1, e2) => s"(${mkString(e1)} / ${mkString(e2)})"

	}

	val optimize: Exp => Exp = exp => exp match {
		case Multiply(e1, e2) if(e1 == e2) => Square(optimize(e1))
		case IntValue(v) => IntValue(v)
		case DecValue(v) => DecValue(v)
		case Sum(e1, e2) => Sum(optimize(e1), optimize(e2))
	}
}