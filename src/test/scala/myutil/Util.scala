package myutil

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._
/**
  *
  */
object Util {

     /*def inspect[T](implicit /*m: Manifest[T],*/ t: TypeTag[T]) =  {
          typeTag[T].tpe.toString.split("\\.").last
     }*/
     def inspect[T: ClassTag](implicit t: TypeTag[T]): String =  {
          typeTag[T].tpe.toString.replace(getPackageName[T], "")
          //.split("\\.").last
     }

     def getPackageName[T: ClassTag]: String ={
          classTag[T].runtimeClass.getPackage match {
               case null => ""
               case other => other.toString.split("\\ ").last + "."
          }
     }

     /*def getNeatType[T: ClassTag, TypeTag]: String ={
          //check if we're dealing with a Primitive (Int ..) or more complicated type (Five [Conj, Disj ...])
          val pack = getPackageName[T]

          .replace(getPackageName[T], "")
     }*/
}
