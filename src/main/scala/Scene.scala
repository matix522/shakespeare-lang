sealed abstract class ScenePart


case class Enter(first: String, second: Option[String]) extends ScenePart

case class Exit(first: String) extends ScenePart

case class Exeunt(first: Option[String], second: Option[String]) extends ScenePart

case class Speaker(first: String) extends ScenePart

case class Sentence(expressions: List[Expression]) extends ScenePart


sealed abstract class Expression
sealed abstract class Value extends Expression
sealed abstract class Condition extends Expression



case class TODOExpression(todo: String) extends Expression { //TODO EXPRESSIONS
    override def toString = s"Some expressions"
}


case class PositiveNoun(speaker : Boolean) extends Value
case class NegativeNoun(speaker : Boolean) extends Value
case class NeutralNoun(speaker : Boolean) extends Value
case class Adjective(value: Value) extends Value

case class JustValue(value: Int) extends Value

case class Sum(a: Value, b: Value) extends Value
case class Difference(a: Value, b: Value) extends Value
case class Product(a: Value, b: Value) extends Value
case class Quotient(a: Value, b: Value) extends Value
case class Remainder(a: Value, b: Value) extends Value

case class Square(a: Value) extends Value
case class SquareRoot(a: Value) extends Value
case class Cube(a: Value) extends Value

case class CharacterValue(speaker : Boolean) extends Value
case class SpecifiedCharacterValue(character: String) extends Value

case class Assigment(speaker : Boolean, value: Value) extends Expression

case class PrintInt(speaker : Boolean) extends Expression
case class LoadInt(speaker : Boolean) extends Expression
case class PrintChar(speaker : Boolean) extends Expression
case class LoadChar(speaker : Boolean) extends Expression

case class GotoS(scene: Int) extends Expression //TODO LOW PRIO
case class GotoA(act: Int) extends Expression //TODO LOW PRIO

case class Push(destCharacter: String, src: Value) extends Expression  //TODO LOW PRIO
case class Pop(destCharacter: String) extends Expression  //TODO LOW PRIO

case class Equal(a: Value, b: Value) extends Condition
case class Less(a: Value, b: Value) extends Condition
case class More(a: Value, b: Value) extends Condition
case class Not(c: Condition) extends Condition

case class ConditionalBlock(condition: Condition, expression: Expression) extends Expression

class Scene(val id: Int, val sceneParts: List[ScenePart]) {

    override def toString = s"\t\nScene(\n\t\tid=$id,\n\t\tsceneParts=$sceneParts\n\t)"
}
