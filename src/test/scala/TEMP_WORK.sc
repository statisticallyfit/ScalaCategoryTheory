import monoid.data._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.TypeRef
import scala.reflect._

def getPackageName[T: ClassTag]: String ={
     classTag[T].runtimeClass.getPackage match {
          case null => ""
          case other => other.toString.split("\\ ").last + "."
     }
}
def inspect[T: ClassTag](implicit t: TypeTag[T]): String =  {
     typeTag[T].tpe.toString.replace(getPackageName[T], "")
     //.split("\\.").last
}


inspect[Five[Trivial, Conjunction,
     Disjunction, ExclusiveDisjunction,
     ExclusiveNorDisjunction]]
inspect[Int]
inspect[Option[Int]]
inspect[Conjunction]
inspect[Two[String, Int]]

/*val pack = classTag[Five[Trivial, Conjunction,
     Disjunction, ExclusiveDisjunction,
     ExclusiveNorDisjunction]].runtimeClass.getPackage match {
     case null => ""
     case other => other.toString.split("\\ ").last + "."
}
typeTag[Five[Trivial, Conjunction,
     Disjunction, ExclusiveDisjunction,
     ExclusiveNorDisjunction]].tpe.toString.replace(pack, "")*/

//---
/*val pack = getPackageName[Five[Trivial, Conjunction,
     Disjunction, ExclusiveDisjunction,
     ExclusiveNorDisjunction]]


inspect[Five[Trivial, Conjunction,
     Disjunction, ExclusiveDisjunction,
     ExclusiveNorDisjunction]].replace(pack, "")*/




/*def detect[T](x: T)(implicit t: TypeTag[T]): String ={
     case TypeRef(args) => args.map(detect)
     case other => inspect(other)
}*/