
class Interpreter(var characters: Map[String, Character], val acts: List[Act]) {
    var stage = new Stage
    var sceneNumber = 1
    var actNumber = 1

    def execute() = {
        while (actNumber < acts.toList.length) {
            val act = acts(actNumber)
            sceneNumber = 1
            while (sceneNumber > 0 && sceneNumber < act.scenes.toList.length) {
                sceneNumber = doScene(act.scenes(sceneNumber))
            }
            actNumber += 1
        }
    }

    def getCharacter(character: String) = {
        if (stage.isOnStage(characters(character))) {
            characters(character)
        }
        else throw new RuntimeException(s"There is no $character on the scene.")
    }

    def doScene(scene: Scene): Int = {
        for (scenePart <- scene.sceneParts) {
            scenePart match {
                case Enter(first, None) => stage.enter(characters(first))
                case Enter(first, second) => stage.enter(characters(first), characters(second))
                case Exeunt(None, None) => stage.exeunt()
                case Exeunt(first, second) => stage.exeunt(characters(first), characters(second))
                case Exit(first) => stage.exit(characters(first))
                case Speaker(first) => stage.changeSpeaker(characters(first))
                case Sentence(expressions) =>
                    val sceneNum = doExpressions(expressions)
                    if (sceneNum != 0) {
                        return sceneNum
                    }
            }
        }
        return sceneNumber + 1
    }

    def doExpressions(expressions: List[Expression]): Int = {
        for (expr <- expressions) {
            expr match {

                case Assigment(character: String, value: Value) => getCharacter(character).value = calculateValue(value)

                case PrintInt(character: String) => print(getCharacter(character).value)
                case LoadInt(character: String) => getCharacter(character).value = Console.in.read.toChar.asInstanceOf[Int]
                case PrintChar(character: String) => print(getCharacter(character).value.asInstanceOf[Char])
                case LoadChar(character: String) => getCharacter(character).value = Console.in.read.toChar

                case GotoS(scene: Int) => {
                    return scene
                }
                case GotoA(act: Int) => {
                    actNumber = act - 1
                    return -1

                }

                case Push(destCharacter: String, src: Value) => {
                    getCharacter(destCharacter).stack.push(calculateValue(src))
                }
                case Pop(destCharacter: String) => {
                    val c = getCharacter(destCharacter)
                    c.value = c.stack.pop()
                }


                case ConditionalBlock(condition: Condition, expression: Expression) =>
                    if (checkCondition(condition)) {
                        expression
                    }
            }
        }
        return 0
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
