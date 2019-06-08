sealed abstract class ScenePart


case class Enter(first: String, second: Option[String]) extends ScenePart
case class Exit(first : String) extends ScenePart
case class Exeunt(first: Option[String], second: Option[String]) extends ScenePart
case class Dialog(expressions: List[Expression] ) extends ScenePart


sealed abstract class Expression
case class TODOExpression(todo: String ) extends Expression { //TODO EXPRESSIONS
    override def toString = s"Some expressions"
}


class Scene (val id : String, val sceneParts: List[ScenePart]){

    override def toString = s"\t\nScene(\n\t\tid=$id,\n\t\tsceneParts=$sceneParts\n\t)"
}
