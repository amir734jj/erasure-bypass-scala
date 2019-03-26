object Driver {
  def main(args: Array[String]): Unit = {

    object Extractor {
      import scala.reflect.ClassTag

      // This will fail because type of TNode and TMatch have not been passes implicitly and
      // they will be erased by type erasure
      def extractFail[TNode, TMatch](list: List[TNode]) = list.filter {
        case _: TMatch => true
        case _ => false
      }.map(x => x.asInstanceOf[TMatch])

      // This will work because we passed type of TNode and TMatch implicitly using ClassTag
      // but fails to differentiate between List[Any] and List[String]
      def extractUsingClassTag[TNode : ClassTag, TMatch : ClassTag](list: List[TNode]) = list.filter {
        case _: TMatch => true
        case _ => false
      }.map(x => x.asInstanceOf[TMatch])

      import scala.reflect.runtime.universe._

      // This is a complete solution as it understands the difference between List[Any] and List[String]
      // and as a result, does not type check fo each item in the list
      def extractUsingTypeTag[TNode : TypeTag : ClassTag, TMatch : TypeTag : ClassTag](list: List[TNode]) = list match {
        case _ if typeOf[TNode] =:= typeOf[TMatch] => list.asInstanceOf[List[TMatch]]
        case _ => extractUsingClassTag[TNode, TMatch](list)
      }
    }

    val homogeneous: List[String] = List("Hello", "World!")
    val heterogeneous: List[Any] = List("Hello", "World!", 123, false)

    println("extractFail")
    println(Extractor.extractFail[Any, String](homogeneous))
    println(Extractor.extractFail[Any, String](heterogeneous) + "\n")

    println("extractUsingClassTag")
    println(Extractor.extractUsingClassTag[Any, String](homogeneous))
    println(Extractor.extractUsingClassTag[Any, String](heterogeneous) + "\n")

    println("extractUsingTypeTag")
    println(Extractor.extractUsingTypeTag[String, String](homogeneous))
    println(Extractor.extractUsingTypeTag[Any, String](heterogeneous) + "\n")
  }
}