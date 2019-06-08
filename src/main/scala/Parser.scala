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
        val id = actCode.split(":")(0).trim()

        //TODO Check if roman numeral

            val act_number = RomanToInt(id)

            if (act_number == -1){

                // error?
            }


        //TODO end


        var scenesCode = actCode.split("Scene ").toList

        if (scenesCode.length < 2) {
            throw new IllegalArgumentException(s"No scenes in act $id ")
        }
        val scenes = scenesCode.slice(1, scenesCode.size).map(parseScene)


        new Act(id, scenes)
    }

    def parseScene(sceneCode: String): Scene = {
        val id = sceneCode.split(":")(0).trim()

        //TODO Check if roman numeral

        val scene_number = RomanToInt(id)

        if (scene_number == -1){

            // error?
        }


        //TODO end


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

    def RomanToInt(roman: String) : Int = {

        if ( !roman.matches("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$")) {
                //incorrect roman number
                return -1
            }


        val roman_numerals_map : Map[Char,Int] = Map('M' -> 1000, 'D' -> 500,
            'C' -> 100, 'L' -> 50, 'X' -> 10, 'V' -> 5, 'I' -> 1)

        var res: Int = 0

        var i = 0
        while ( i < roman.length) {

            val s1 = roman_numerals_map(roman.charAt(i))

            if (i + 1 < roman.length) {
                val s2 = roman_numerals_map(roman.charAt(i + 1))

                if (s1 >= s2) {
                    res = res + s1
                }
                else {
                    res = res + s2 - s1
                    i += 1

                }
            }
            else {
                res = res + s1
                i += 1
            }

            i += 1
        }

        res
    }
}
