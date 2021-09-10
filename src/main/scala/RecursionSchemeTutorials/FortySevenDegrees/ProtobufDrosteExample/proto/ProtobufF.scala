package RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto

/**
 * GOAL: parser as an anamorphism in Skeuomorph: https://hyp.is/PtLH0BFrEeyTPA_QK0rIig/www.47deg.com/blog/recursion-schemes-introduction/
 *
 * SOURCE OF FILE = https://github.com/higherkindness/ersatz/blob/master/src/main/scala/io/higherkindness/ersatz/proto
 * /schema.scala
 */


import cats._
import cats.implicits._
import higherkindness.droste.util.DefaultTraverse


// TODO why is this same as Field case class in SchemaF?
sealed trait FieldF[A] {
	val name: String
	val tpe: A
}

object FieldF {
	final case class Field[A](name: String, tpe: A, position: Int, isRepeated: Boolean) extends FieldF[A]

	final case class OneOfField[A](name: String, tpe: A) extends FieldF[A]
}


/**
 * Must write a pattern functor for our structure
 */
sealed trait ProtobufF[A]

object ProtobufF {
	final case class TNull[A]() extends ProtobufF[A]
	final case class TUint64[A]() extends ProtobufF[A]
	final case class TBool[A]() extends ProtobufF[A]
	final case class TString[A]() extends ProtobufF[A]
	final case class TNamedType[A](name: String) extends ProtobufF[A]
	final case class TRepeated[A](value: A) extends ProtobufF[A]
	final case class TMessage[A](name: String, fields: List[FieldF[A]]) extends ProtobufF[A]


	// Smart constructors to avoid scala inferring specific types instead of SchemaF
	def uint64[A](): ProtobufF[A] = TUint64()
	def bool[A](): ProtobufF[A] = TBool()
	def string[A](): ProtobufF[A] = TString()
	def namedType[A](name: String): ProtobufF[A] = TNamedType(name)
	def repeated[A](value: A): ProtobufF[A] = TRepeated(value)
	def message[A](name: String, fields: List[FieldF[A]]): ProtobufF[A] = TMessage(name, fields)


	implicit val traverse: DefaultTraverse[ProtobufF] = new DefaultTraverse[ProtobufF] {
		def traverse[G[_], A, B](protoFA: ProtobufF[A])(f: A => G[B])(implicit gapplic: Applicative[G])
		: G[ProtobufF[B]]	= {

			def makeFieldB(field: FieldF.Field[A]): G[FieldF.Field[B]] =
				f(field.tpe).map(
					b => FieldF.Field[B](
						name = field.name,
						tpe = b,
						position = field.position,
						isRepeated = field.isRepeated
					)
				)

			def traverseFieldF(fieldFList: List[FieldF[A]]): G[List[FieldF[B]]] =
				fieldFList.traverse { // takes FieldF.Field[A] as argument
					case field: FieldF.Field[A] => makeFieldB(field).widen
					// TODO what does widen do?
					// TODO does  widen :: G[FieldF.Field[B]] => G[List[FieldF[B]] ?
					// so does it convert the inner type from FieldF.Field[B] ===> List[FieldF[B]] ?
					// if so how does it get rid of the FieldF.Field part and keep just FieldF?
				}

			protoFA match {
				case TUint64() => uint64[B]().pure[G] // returns G[TUint64[B]]
				case TBool() => bool[B]().pure[G] // returns G[TBool[B]]
				case TString() => string[B]().pure[G]	// returns G[TString[B]]
				case TNamedType(name) => namedType[B](name).pure[G] //returns G[TNamedType[B]]
					// NOTE: value :: A ---> f(value) :: G[B] ----> .map(b => repeated[B](b)) :: G[Protobuf[B]]
				case TRepeated(value) => f(value).map(b => repeated[B](b))

					// NOTE: this converts each list inside G[] into a TMessage
				case TMessage(string, fields: List[FieldF[A]]) =>
					traverseFieldF(fields).map(listFieldB => TMessage[B](string, listFieldB))
					//returns G[TMessage[B]]
			}
		}

	}
}
