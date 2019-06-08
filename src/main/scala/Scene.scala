sealed abstract class ScenePart


case class Enter(first: String, second: Option[String]) extends ScenePart

case class Exit(first: String) extends ScenePart

case class Exeunt(first: Option[String], second: Option[String]) extends ScenePart

case class Speaker(first: String) extends ScenePart

case class Sentence(expressions: List[Expression]) extends ScenePart


sealed abstract class Expression
sealed abstract class Value extends Expression
sealed abstract class Boolean extends Expression

case class TODOExpression(todo: String) extends Expression { //TODO EXPRESSIONS
    override def toString = s"Some expressions"
}

case class PositiveNoun(todo: String) extends Value
case class NegativeNoun(todo: String) extends Value
case class NeutralNoun(todo: String) extends Value
case class Adjective(value: Value) extends Value

case class Sum(a: Value, b: Value) extends Value
case class Difference(a: Value, b: Value) extends Value
case class Product(a: Value, b: Value) extends Value
case class Quotient(a: Value, b: Value) extends Value
case class Remainder(a: Value, b: Value) extends Value

case class Square(a: Value) extends Value
case class SquareRoot(a: Value) extends Value

case class CharacterValue(character: Character) extends Value

case class Assigment(character: Character, value: Value) extends Expression

case class PrintInt(dest: Character) extends Expression
case class LoadInt(dest: Character) extends Expression
case class PrintChar(dest: Character) extends Expression
case class LoadChar(dest: Character) extends Expression

case class GotoS(scene: Scene) extends Expression
case class GotoA(scene: Act) extends Expression

case class Push(dest: Character, src: Value) extends Expression
case class Pop(dest: Character) extends Expression

case class Equal(a: Value, b: Value) extends Boolean
case class Less(a: Value, b: Value) extends Boolean
case class More(a: Value, b: Value) extends Boolean
case class Not(b: Boolean) extends Boolean

case class ConditionalBlock(condition: Boolean, expression: Expression) extends Expression

class Scene(val id: String, val sceneParts: List[ScenePart]) {

    override def toString = s"\t\nScene(\n\t\tid=$id,\n\t\tsceneParts=$sceneParts\n\t)"
}
