import scala.collection.mutable.ListBuffer

class Parser(val sourceCode: String, val dictionary: Dictionary) {

    def parse(): Unit = {
        var actsCode = sourceCode.split("Act ").toList
        if (actsCode.length < 2) {
            throw new IllegalArgumentException("No acts in source code")
        }
        val charactersList = parseCharacters(actsCode(0))
        println("Characters:" + charactersList)

        //println(actsCode)
        val acts = actsCode.slice(1, actsCode.size).map(parseAct).toMap

        println(acts)
    }

    def parseCharacters(charactersList: String): List[Character] = {
        charactersList.trim.split("\n").toList.map(s => new Character(s.split(",")(0), 0))
    }

    def parseAct(actCode: String): (Int,Act) = {
        val id = RomanToInt(actCode.split(":")(0).trim())

        var scenesCode = actCode.split("Scene ").toList

        if (scenesCode.length < 2) {
            throw new IllegalArgumentException(s"No scenes in act $id ")
        }
        val scenes = scenesCode.slice(1, scenesCode.size).map(parseScene).toMap

        (id, new Act(id, scenes))
    }

    def parseScene(sceneCode: String): (Int, Scene) = {
        val id = RomanToInt(sceneCode.split(":")(0).trim())
        val sentences = sceneCode.split(".")
        val code = sentences.slice(1,sentences.length).mkString(".")

        val sceneParts = new ListBuffer[ScenePart]

        val enter = "\\[((Enter)|(Exit)|(Exeunt))( [A-Z][a-z]*)?( and )?([A-Z][a-z]*)?\\]"
        val enterRegex = enter.r

        val dialogs = code.split(enter).toList.map(s => s.trim())
        var i = 0
        for (regMatch <- enterRegex.findAllMatchIn(code)) {
            val enterExitBlock = regMatch match {
                case enterRegex("Exeunt", _, _, _, first, " and ", second) => Exeunt(Some(first.trim), Some(second.trim))
                case enterRegex("Exeunt", _, _, _, _, _, _) => Exeunt(None, None)
                case enterRegex("Enter", _, _, _, first, " and ", second) => Enter(first, Some(second.trim))
                case enterRegex("Enter", _, _, _, first, _, _) => Enter(first, None)
                case enterRegex("Exit", _, _, _, first, _, _) => Exit(first)
                case _ => throw new IllegalArgumentException(s"Incorect [Enter/Exit] block $regMatch")
            }

            // TODO somehow add speaker
            //sceneParts.addOne(Sentence(List(TODOExpression(dialogs(i)))))


            sceneParts.addAll(parse_statements(dialogs(i)))

            i += 1
            sceneParts.addOne(enterExitBlock)
        }

        (id, new Scene(id, sceneParts.toList))
    }

    def parse_statements(s: String): List[ScenePart] = {

        var characters = s.split("\n[A-Z,a-z]*:\n")

        var ret = new ListBuffer[ScenePart]
        for (c <- characters) {
            var character_sentences = c.split(":")

            var character = character_sentences(0).trim()

            var sentences = character_sentences(1)

            if (!dictionary.character.contains(character)) {

                throw new IllegalArgumentException(s"ERROR! Character $character is not an Shakespeare character!")
            }

            ret += Speaker(character)

            ret += parse_sentences(sentences, character)

        }


        ret.toList

    }

    def parse_sentences(str: String, character: String): Sentence = {

        var ret = new ListBuffer[Expression]

        val sentences = str.split(". ")

        for (s <- sentences) {

            if (s.matches("Open .*")) {

                var tokens = s.split(" ")

                val possesive = tokens(1)

                if (dictionary.first_person_possessive.contains(possesive)) {
                    ret.addOne(PrintInt(true))


                }
                else if (dictionary.second_person_possessive.contains((possesive))) {
                    ret.addOne(PrintInt(false))


                }

                else throw new IllegalArgumentException(s"Error, $possesive is not a correct possesive word ")
            }
             else  if (s.matches("Speak .*")){

              var tokens = s.split(" ")

              val possesive = tokens(1)

              if (dictionary.first_person_possessive.contains(possesive)) {
                ret.addOne(PrintChar(true))


              }
              else if (dictionary.second_person_possessive.contains((possesive))) {
                ret.addOne(PrintChar(false))

              }

              else throw new IllegalArgumentException(s"Error, $possesive is not a correct possesive word ")
            }


        }

        Sentence(ret.toList)
    }


    def RomanToInt(roman: String): Int = {
        if (!roman.matches("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$")) {
           throw new IllegalArgumentException( s"$roman is not roman numeral")
        }


        val roman_numerals_map: Map[Char, Int] = Map('M' -> 1000, 'D' -> 500,
            'C' -> 100, 'L' -> 50, 'X' -> 10, 'V' -> 5, 'I' -> 1)

        var res: Int = 0

        var i = 0
        while (i < roman.length) {

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
