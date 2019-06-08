import scala.collection.mutable.ListBuffer

class Parser(val sourceCode: String) {

    def parse() = {
        var actsCode = sourceCode.split("Act ").toList
        if (actsCode.length < 2) {
            throw new IllegalArgumentException("No acts in source code")
        }
        val charactersList = parseCharacters(actsCode(0))
        println("Characters:" + charactersList)

        //println(actsCode)
        val acts = actsCode.slice(1, actsCode.size).map(parseAct)

        println(acts)
    }

    def parseCharacters(charactersList: String): List[Character] = {
        charactersList.trim.split("\n").toList.map(s => new Character(s.split(",")(0), 0))
    }

    def parseAct(actCode: String): Act = {
        val id = actCode.split(":")(0).trim() //TODO Check if roman numeral
        var scenesCode = actCode.split("Scene ").toList

        if (scenesCode.length < 2) {
            throw new IllegalArgumentException(s"No scenes in act $id ")
        }
        val scenes = scenesCode.slice(1, scenesCode.size).map(parseScene)


        return new Act(id, scenes)
    }

    def parseScene(sceneCode: String): Scene = {
        val id = sceneCode.split(":")(0).trim() //TODO Check if roman numeral

        val sceneParts = new ListBuffer[ScenePart]

        val enter = "\\[((Enter)|(Exit)|(Exeunt))( [A-Z][a-z]*)?( and )?([A-Z][a-z]*)?\\]"
        val enterRegex = enter.r

        val dialogs = sceneCode.split(enter).toList.map(s => s.trim())
        var i = 0
        for (regMatch <- enterRegex.findAllMatchIn(sceneCode)) {
            val enterExitBlock = regMatch match {
                case enterRegex("Exeunt", _, _, _, first, " and ", second) => Exeunt(Some(first.trim), Some(second.trim))
                case enterRegex("Exeunt", _, _, _, _, _, _) => Exeunt(None, None)
                case enterRegex("Enter", _, _, _, first, " and ", second) => Enter(first, Some(second.trim))
                case enterRegex("Enter", _, _, _, first, _, _) => Enter(first, None)
                case enterRegex("Exit", _, _, _, first, _, _) => Exit(first)
                case _ => throw new IllegalArgumentException(s"Incorect [Enter/Exit] block $regMatch")
            }
            sceneParts.addOne(Sentence(List(TODOExpression(dialogs(i)))))
            i += 1
            sceneParts.addOne(enterExitBlock)
        }

        new Scene(id, sceneParts.toList)
    }

    def isRomanNumeral(roman: String) = {
        true //TODO Check if roman numeral
    }
}
