import scala.collection.mutable
class Character (val name: String, var value : Int){
    var stack = new mutable.Stack[Int]()
    override def toString = s"$name: $value, Stack: $stack"
}
