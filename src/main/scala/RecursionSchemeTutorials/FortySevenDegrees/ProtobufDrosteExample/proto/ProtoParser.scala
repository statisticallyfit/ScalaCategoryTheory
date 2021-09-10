package RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto



import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.DescriptorProtos.{DescriptorProto, FieldDescriptorProto, FileDescriptorProto}

import higherkindness.droste.{Coalgebra, Embed, scheme}
import higherkindness.droste.syntax.embed._

import RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto.ProtobufF.{TBool, TNamedType, TNull, TString, TUint64}
import RecursionSchemeTutorials.FortySevenDegrees.ProtobufDrosteExample.proto.protocol.Protocol


import scala.collection.JavaConverters._
/**
 *
 */
object ProtoParser {


	def findDescriptorProto(name: String, files: List[FileDescriptorProto]): Option[FileDescriptorProto] =
		files.find(fileDescrProto => fileDescrProto.getName == name )




	case class NamedMessage(fullName: String, msg: DescriptorProto)

	def namedMessages(fileDescrProto: FileDescriptorProto): List[NamedMessage] = {
		fileDescrProto.getMessageTypeList.asScala.toList.flatMap(m =>
			NamedMessage(
				fullName = s".${fileDescrProto.getPackage}.${m.getName}",
				msg = m
			) :: m.getNestedTypeList.asScala.toList.map(n =>
				NamedMessage(
					fullName = s".${fileDescrProto.getPackage}.${m.getName}.${n.getName}",
					msg = n
				)
			)
		)
	}


	def findMessage(name: String, namedMessages: List[NamedMessage]): Option[DescriptorProto] = {
		//maps through the first Option returned, gets the .msg which is a DescriptorProto
		namedMessages.find(nm => nm.fullName == name).map(_.msg)
	}


	def fromFieldTypeCoalgebra(field: FieldDescriptorProto, namedMessages: List[NamedMessage]): Coalgebra[ProtobufF, Type] = Coalgebra {

		case Type.TYPE_BOOL => TBool()
		case Type.TYPE_STRING => TString()
		case Type.TYPE_UINT64 => TUint64()
		case Type.TYPE_MESSAGE =>
			findMessage(name = field.getTypeName, namedMessages = namedMessages)
			.fold[ProtobufF[Type]](TNull())(descrProto => TNamedType(descrProto.getName))
	}




	def fromFieldType[A](
						field: FieldDescriptorProto,
						namedMessages: List[NamedMessage])(implicit ev: Embed[ProtobufF, A]): A = {

		// NOTE: anamorphism
		scheme.ana(coalgebra = fromFieldTypeCoalgebra(field, namedMessages)).apply(field.getType)
	}




	def fromFieldDescriptorProto[A](field: FieldDescriptorProto, namedMessages: List[NamedMessage])(implicit ev: Embed[ProtobufF, A]): FieldF[A] = {

		FieldF.Field(
			name = field.getName,
			tpe = fromFieldType(field = field, namedMessages = namedMessages),
			position = field.getNumber,
			isRepeated = field.getLabel.isRepeated
		)
	}



	def fromDescriptor[A](descriptor: FileDescriptorProto)(implicit A: Embed[ProtobufF, A]): Protocol[A] = {
		val named: List[NamedMessage] = namedMessages(descriptor)
		val messages: List[A]  = descriptor.getMessageTypeList.asScala.toList.map(
			m => ProtobufF.message[A](
				name = m.getName,
				fields = m.getFieldList.asScala.toList.map(
					field => fromFieldDescriptorProto(field = field, namedMessages = named)
				)
			).embed
		)

		Protocol[A](name = descriptor.getName, messages = messages)
	}



	implicit class LabelOps(self: Label) {
		def isRepeated: Boolean = self.name() == "LABEL_REPEATED"
	}
}
