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

        //println(acts)

        // interpreter testing

//        for (i <- charactersList)
//            println(i)

        val charactersMap = charactersList.map(a => a.name.toLowerCase.trim -> a).toMap
//
//        for (i <- charactersMap)
//            println(i._1)

        val interpreter = new Interpreter(charactersMap,acts)

        interpreter.execute()



    }

    def parseCharacters(charactersList: String): List[Character] = {
        charactersList.trim.split("\n").toList.map(s => new Character(s.split(",")(0).toLowerCase.trim, 0))
    }

    def parseAct(actCode: String): (Int, Act) = {
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
        val sentences = sceneCode.split("\\.")
        val code = sentences.slice(1, sentences.length).mkString(".")

        val sceneParts = new ListBuffer[ScenePart]

        val enter = "\\[((Enter)|(Exit)|(Exeunt))( [A-Z][a-z]*)?( and )?([A-Z][a-z]*)?\\]"
        val enterRegex = enter.r

        val dialogs = code.split(enter).toList.map(s => s.trim())
        var i = 0
        for (regMatch <- enterRegex.findAllMatchIn(code)) {
            val enterExitBlock = regMatch match {
                case enterRegex("Exeunt", _, _, _, first, " and ", second) => Exeunt(Some(first.trim.toLowerCase), Some(second.trim.toLowerCase))
                case enterRegex("Exeunt", _, _, _, _, _, _) => Exeunt(None, None)
                case enterRegex("Enter", _, _, _, first, " and ", second) => Enter(first.toLowerCase.trim, Some(second.trim.toLowerCase))
                case enterRegex("Enter", _, _, _, first, _, _) => Enter(first.toLowerCase.trim, None)
                case enterRegex("Exit", _, _, _, first, _, _) => Exit(first.toLowerCase.trim)
                case _ => throw new IllegalArgumentException(s"Incorect [Enter/Exit] block $regMatch")
            }

            sceneParts.addAll(parse_statements(dialogs(i)))

            i += 1
            sceneParts.addOne(enterExitBlock)
        }
        (id, new Scene(id, sceneParts.toList))
    }

    def parse_statements(s: String): List[ScenePart] = {
        var sentences = s.split("[A-Z,a-z]*:").filter(s => s.length > 0)
          .map(a => a.replaceAll("\n", " ")).map(a => a.toLowerCase).map(a => a.trim)
            .toList


        var characters = "[A-Z,a-z]*:".r
            .findAllMatchIn(s)
            .map(m => m.group(0).replace(":", "")).map(a => a.toLowerCase.trim)
            .map(s => {
                if (!dictionary.character.contains(s)) throw new IllegalArgumentException(s"ERROR! Character $s is not an Shakespeare character!")
                s
            })
            .toList

        var ret = new ListBuffer[ScenePart]
        for ((c, s) <- characters.map(s => Speaker(s)).zip(sentences.map(s => parse_sentences(s.replace("\r\n", " "))))) {
            ret.addOne(c)
            ret.addOne(s)
        }
        //        println(characters)
        //        print(s)
        ret.toList
    }

    def parse_sentences(str: String): Sentence = {

        var ret = new ListBuffer[Expression]

     // println(str)

        val sentences = str.split("\\.|!|\\?").map(a => a.replaceAll("\n", " ")).map(a => a.toLowerCase).map(a => a.trim).toList
        for (s <- sentences) {
         // println(s)
            val printInt = "(open) (.*) (heart)".r.findFirstMatchIn(s)
            if (printInt.nonEmpty) {

                val possessive =  printInt.get.group(2).toLowerCase

                if (dictionary.first_person_possessive.contains(possessive))
                    ret.addOne(PrintInt(true))
                else if (dictionary.second_person_possessive.contains(possessive))
                    ret.addOne(PrintInt(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possesive word ")


            }
            val printChar = "(speak) (.*) (.)".r.findFirstMatchIn(s)
            if (printChar.nonEmpty) {

                val possessive = printChar.get.group(2).toLowerCase

                if (dictionary.first_person_possessive.contains(possessive))
                    ret.addOne(PrintChar(true))
                else if (dictionary.second_person_possessive.contains(possessive))
                    ret.addOne(PrintChar(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possessive word ")
            }

            var tokens = s.split(" ")

            var assig = tokens(0)

            if (dictionary.first_person.contains(assig)) {

                ret.addOne(Assigment(speaker = true,get_value(tokens)))

            }

            else if (dictionary.second_person.contains(assig)) {

                ret.addOne(Assigment(speaker = false,get_value(tokens)))

            }




          //TODO

          //pomijac ewentualne are as * as



          //napisac funkcje, ktora ogarnie wartosci wyrazen


        }

        Sentence(ret.toList)
    }


    def normal_value(strings: Array[String], i: Int): Value = {

        if (i == strings.length)
            throw new IllegalArgumentException("Error in sentence")

        val word = strings(i)

        // a an or the or my, mine, yours....
        if (dictionary.article.contains(word)
        || dictionary.first_person_possessive.contains(word)
        || dictionary.second_person_possessive.contains(word)
        || dictionary.third_person_possessive.contains(word)){
            return normal_value(strings, i+1)
        }

        if (dictionary.negative_adjective.contains(word) ||
             dictionary.neutral_adjective.contains(word) ||
            dictionary.positive_adjective.contains(word))
                return Adjective(normal_value(strings,i+1))

        if (dictionary.negative_noun.contains(word))
            return NegativeNoun(true)

        if (dictionary.neutral_noun.contains(word))
            return NeutralNoun(true)

        if (dictionary.positive_noun.contains(word))
            return PositiveNoun(true)

        if (dictionary.nothing.contains(word))
            return JustValue(0)

        throw new IllegalArgumentException(s"Error in word $word")


    }

    def get_value(strings: Array[String]) : Value = {

       //return JustValue(70)

        var i = 1

        val be = strings(i)

        if (dictionary.be.contains(be)){
           i+=1
            //throw new IllegalArgumentException(s"Error! $be is not a correct be word")
        }

        //i +=1

        if (strings(i) != "as"){

            return normal_value(strings,i)
        }

        i+=3



        return JustValue(60)







    }





    def RomanToInt(roman: String): Int = {
        if (!roman.matches("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$")) {
            throw new IllegalArgumentException(s"$roman is not roman numeral")
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
