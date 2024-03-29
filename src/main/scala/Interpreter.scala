
class Interpreter(var characters: Map[String, Character], val acts: Map[Int, Act]) {
    var stage = new Stage
    var sceneNumber = 1
    var actNumber = 1

    var was_condition = false
    var condition_was = false

    var debug = false

    def execute(debug : Boolean = false): Unit = {
        this.debug = debug
        while (actNumber <= acts.toList.length) {
            val act = acts(actNumber)
            if(debug)
                println("Act " + actNumber)

            sceneNumber = 1
            while (sceneNumber > 0 && sceneNumber <= act.scenes.toList.length) {
                sceneNumber = debug match {
                    case true => doDebugScene(act.scenes(sceneNumber))
                    case false=> doScene(act.scenes(sceneNumber))
                }
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

    def doScene(scene: Scene): Int = {
        for (scenePart <- scene.sceneParts) {
            scenePart match {
                case Enter(first, None) =>  stage.enter(characters(first))
                case Enter(first, second) => stage.enter(characters(first), characters(second.get))
                case Exeunt(None, None) => stage.exeunt()
                case Exeunt(first, second) => stage.exeunt(characters(first.get), characters(second.get))
                case Exit(first) => stage.exit(characters(first))
                case Speaker(first) => stage.changeSpeaker(characters(first))
                case Sentence(expressions) =>
                    val sceneNum = doExpressions(expressions)
                    if (sceneNum != 0) {
                        return sceneNum
                    }
            }
        }
        sceneNumber + 1
    }
    def doDebugScene(scene: Scene): Int = {
        println("\tScene " + sceneNumber)

        for (scenePart <- scene.sceneParts) {
            scenePart match {
                case Enter(first, None) =>
                    println("\t\t" + scenePart);
                    stage.enter(characters(first))
                case Enter(first, second) =>
                    println("\t\t" + scenePart);
                    stage.enter(characters(first), characters(second.get))
                case Exeunt(None, None) =>
                    println("\t\t" + scenePart);
                    stage.exeunt()
                case Exeunt(first, second) =>
                    println("\t\t" + scenePart);
                    stage.exeunt(characters(first.get), characters(second.get))
                case Exit(first) =>
                    println("\t\t" + scenePart);
                    stage.exit(characters(first))
                case Speaker(first) =>
                    println("\t\t" + scenePart);
                    println(s"\t\t$stage")
                    stage.changeSpeaker(characters(first))
                case Sentence(expressions) =>
                    println("\t\t  Sentence:")
                    expressions.foreach(e => println("\t\t\t" + e))
                    val sceneNum = doExpressions(expressions)
                    if (sceneNum != 0) {
                        return sceneNum
                    }
            }
        }
        sceneNumber + 1
    }

    def doExpressions(expressions: List[Expression]): Int = {

        for (expr <- expressions) {
            expr match {

                case Assigment(speaker: Boolean, value: Value) => getCharacter(if (speaker) stage.speaker.get.name else stage.listener.get.name).value = calculateValue(value)

                case PrintInt(speaker: Boolean) => print(getCharacter(if (speaker) stage.speaker.get.name else stage.listener.get.name).value)
                case LoadInt(speaker: Boolean) => getCharacter(if (speaker) stage.speaker.get.name else stage.listener.get.name).value = scala.io.StdIn.readLine().toInt
                case PrintChar(speaker: Boolean) => print(getCharacter(if (speaker) stage.speaker.get.name else stage.listener.get.name).value.asInstanceOf[Char])
                case LoadChar(speaker: Boolean) => getCharacter(if (speaker) stage.speaker.get.name else stage.listener.get.name).value = Console.in.read.toChar

                case GotoS(scene: Int) =>
                    return scene
                case GotoA(act: Int) =>
                    actNumber = act - 1
                    return -1

                case Push(speaker: Boolean) =>
                    getCharacter(if (speaker) stage.getSpeaker.name else stage.getListener.name)
                        .stack.push(if (speaker) stage.getSpeaker.value else stage.getListener.value)
                case Pop() =>
                    val c = getCharacter(stage.getListener.name)
                    c.value = c.stack.top
                    c.stack.pop()


                case Then(if_not: Boolean, expression: Expression) => {
                    if (if_not) {

                        if (!condition_was) {
                            val ret = doExpressions(List(expression))
                            return ret
                        }

                    }
                    else if (!if_not) {
                        if (condition_was) {
                            val ret = doExpressions(List(expression))
                            return ret
                        }

                    }
                }


                case ConditionalBlock(condition: Condition) =>
                    was_condition = true
                    if (checkCondition(condition)) {
                        condition_was = true
                    }
                    else condition_was = false

            }
        }
        0
    }

    def checkCondition(condition: Condition): Boolean = {
        condition match {
            case Equal(a: Value, b: Value) => calculateValue(a) == calculateValue(b)
            case Less(a: Value, b: Value) => calculateValue(a) < calculateValue(b)
            case More(a: Value, b: Value) => calculateValue(a) > calculateValue(b)
            case Not(c: Condition) => !checkCondition(c)
        }
    }

    def sqr(a: Int): Int = {
        a * a
    }

    def sqrt(a: Int): Int = {
        math.sqrt(a).toInt
    }

    def cube(i: Int): Int = {
        i * i * i
    }

    def calculateValue(v: Value): Int = v match {
        case PositiveNoun(_) => 1
        case NegativeNoun(_) => -1
        case NeutralNoun(_) => 1
        case Adjective(value: Value) => 2 * calculateValue(value)

        case JustValue(value: Int) => value

        case Sum(a: Value, b: Value) => calculateValue(a) + calculateValue(b)
        case Difference(a: Value, b: Value) => calculateValue(a) - calculateValue(b)
        case Product(a: Value, b: Value) => calculateValue(a) * calculateValue(b)
        case Quotient(a: Value, b: Value) => calculateValue(a) / calculateValue(b)
        case Remainder(a: Value, b: Value) => calculateValue(a) % calculateValue(b)

        case Square(a: Value) => sqr(calculateValue(a))
        case SquareRoot(a: Value) => sqrt(calculateValue(a))

        case Cube(a: Value) => cube(calculateValue(a))

        case SpecifiedCharacterValue(character: String) =>
            characters(character).value

        case CharacterValue(speaker: Boolean) =>
            if (stage.speaker.nonEmpty && speaker) stage.speaker.get.value
            else if (stage.listener.nonEmpty && !speaker) stage.listener.get.value
            else throw new RuntimeException(s"There is no requested character on the scene.")

    }
}
