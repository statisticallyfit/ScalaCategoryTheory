package RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample

/**
 *
 */

import higherkindness.droste.{Embed, Trans}
import RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto.ProtobufF



object Transform {


	import SchemaF._


	/**
	 * Transform Protobuf schema into SchemaF
	 */

	def transformProto[A](implicit ev: Embed[SchemaF, A]): Trans[ProtobufF, SchemaF, A] = {
		Trans {
			case ProtobufF.TNull() => SchemaF.TNull()
			case ProtobufF.TUint64() => SchemaF.TLong()
			case ProtobufF.TBool() => SchemaF.TBoolean()
			case ProtobufF.TString() => SchemaF.TString()
			case ProtobufF.TNamedType(name) => SchemaF.TOption(ev.algebra(SchemaF.TNamedType(name)))
			case ProtobufF.TRepeated(value) => SchemaF.TList(value)
			case ProtobufF.TMessage(name, fieldsF) => SchemaF.TProduct(
				name = name,
				fields = fieldsF.map(f => Field(name = f.name, tpe = f.tpe))
			)
		}
	}
}
