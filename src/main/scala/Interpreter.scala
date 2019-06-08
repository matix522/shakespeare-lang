
class Interpreter(var characters: Map[String, Character], val acts: List[Act]) {
    var stage = new Stage

    def execute(): Unit = {
        var actNumber = 0


        while (actNumber < acts.length) {
            var sceneNumber = 0
            val act = acts(actNumber)
            while (sceneNumber < act.scenes.length) {
                val scene = act.scenes(sceneNumber)
                for (scenePart <- scene.sceneParts) {
                    scenePart match {
                        case Enter(first, None) => stage.enter(characters(first))
                        case Enter(first, second) => stage.enter(characters(first), characters(second.get))
                        case Exeunt(None, None) => stage.exeunt()
                        case Exeunt(first, second) => stage.exeunt(characters(first.get), characters(second.get))
                        case Exit(first) => stage.exit(characters(first))
                        case Speaker(first) => stage.changeSpeaker(characters(first))
                        case Sentence(expressions) => doExpressions(expressions)
                    }
                }
                sceneNumber += 1
            }
            actNumber += 1
        }
    }

    def getCharacter(character: String): Character = {
        if (stage.isOnStage(characters(character))) {
            characters(character)
        }
        else throw new RuntimeException(s"There is no $character on the scene.")
    }

    def doExpressions(expressions: List[Expression]): Unit = {
        for (expr <- expressions) {
            expr match {

                case Assigment(character: String, value: Value) => getCharacter(character).value = calculateValue(value)

                case PrintInt(character: String) => print(getCharacter(character).value)
                case LoadInt(character: String) =>getCharacter(character).value = Console.in.read.toChar.asInstanceOf[Int]
                case PrintChar(character: String) => print(getCharacter(character).value.asInstanceOf[Char])
                case LoadChar(character: String) =>  getCharacter(character).value = Console.in.read.toChar

                case GotoS(scene: Scene) => //TODO LOW PRIO
                case GotoA(scene: Act) => //TODO LOW PRIO

                case Push(destCharacter: String, src: Value) => //TODO LOW PRIO
                case Pop(destCharacter: String) => //TODO LOW PRIO


                case ConditionalBlock(condition: Condition, expression: Expression) =>
            }
        }
    }

    def checkCondition(condition: Condition): Boolean = {
        condition match {
            case Equal(a: Value, b: Value) => calculateValue(a) == calculateValue(b)
            case Less(a: Value, b: Value) => calculateValue(a) < calculateValue(b)
            case More(a: Value, b: Value) => calculateValue(a) > calculateValue(b)
            case Not(c: Condition) => checkCondition(c)
        }
    }

    def sqr(a: Int): Int = {
        a * a
    }

    def sqrt(a: Int): Int = {
        math.sqrt(a).toInt
    }

    def calculateValue(v: Value): Int = v match {
        case PositiveNoun(_) => 1
        case NegativeNoun(_) => 1
        case NeutralNoun(_) => -1
        case Adjective(value: Value) => 2 * calculateValue(value)

        case Sum(a: Value, b: Value) => calculateValue(a) + calculateValue(b)
        case Difference(a: Value, b: Value) => calculateValue(a) - calculateValue(b)
        case Product(a: Value, b: Value) => calculateValue(a) * calculateValue(b)
        case Quotient(a: Value, b: Value) => calculateValue(a) / calculateValue(b)
        case Remainder(a: Value, b: Value) => calculateValue(a) % calculateValue(b)

        case Square(a: Value) => sqr(calculateValue(a))
        case SquareRoot(a: Value) => sqrt(calculateValue(a))

        case CharacterValue(character: String) =>
            if (stage.isOnStage(characters(character))) {
                characters(character).value
            }
            else throw new RuntimeException(s"There is no $character on the scene.")
    }
}
