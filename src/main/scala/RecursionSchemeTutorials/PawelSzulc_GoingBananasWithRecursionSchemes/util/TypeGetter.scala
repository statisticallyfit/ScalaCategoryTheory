package RecursionSchemeTutorials.PawelSzulc_GoingBananasWithRecursionSchemes.util

import slamdata.Predef.String

import scala.reflect.runtime.universe._
/**
 *
 */

object TypeGetter {


	def typeof[T: TypeTag](obj: T) = typeOf[T]


	def typeClean[T: TypeTag](obj: T): String = {

		val r1a = typeof(obj).toString.replace("Product with Serializable ", "")
		//val r1a = typeof(obj).toString.replace("Product with Serializable with ", "")
		val r1b = if(r1a.contains("[Product with Serializable]")) r1a else r1a.replace("Product with Serializable]", "]")
		val r2 = r1b.replace("_ >: ", "")
		val r3 = r2
			.replace("Object","")
			.replace("_ ", "")
			.replace(" <: ", "")
			.replace("<: ", "")
			.replace(" <:", "")
		val r4 = r3.replace("[with ", "[")
		val r5 = r4.replace("]with", "] with")

		val withAtFront: String = "with "

		val r6 = r5.substring(0, withAtFront.length) == withAtFront match {
			case true => r5.substring(withAtFront.length)
			case false => r5
		}

		val tempStr = r6


		val pkgs = tempStr
			.split("[\\[\\]]")
			.flatMap(e => e.split(' '))
			.filter(e => e.contains('.'))

		val oldNewReplacements = pkgs.distinct.map(p => (p, p.split('.').last))

		oldNewReplacements.foldLeft(tempStr)(
			(currentStrType, tuple) => tuple match {
				case (oldPkg: String, newShort: String) => currentStrType.replaceAll(oldPkg, newShort)
			}
		)
	}

}

