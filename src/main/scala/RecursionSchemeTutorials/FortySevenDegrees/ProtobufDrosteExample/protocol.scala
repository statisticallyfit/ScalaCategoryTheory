package RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample

/**
 *
 */

import higherkindness.droste.{Basis, scheme}
import RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.Transform.transformProto
import RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto.ProtobufF
import RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto.protocol.{Protocol => ProtoProtocol}


final case class protocol[T](name: String, declarations: List[T])

object protocol {

	def fromProtobufProtocol[T, U](protocol: ProtoProtocol[T])(implicit T: Basis[ProtobufF, T], U: Basis[SchemaF, U]): protocol[U] = {

		//NOTE: the `cata` will traverse the data structure and return the result in SchemaF terms
		val toSchemaF: T => U = scheme.cata(transformProto[U].algebra)

		new protocol[U](
			name = protocol.name,
			declarations = protocol.messages.map(m => toSchemaF(m)),
		)
	}
}
