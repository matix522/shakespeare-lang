import scala.collection.mutable.ListBuffer

class Parser(val sourceCode: String, val dictionary: Dictionary) {

    def parse(): Unit = {
        val actsCode = sourceCode.split("Act ").toList
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
        val id = RomanToInt(actCode.split(":")(0).trim().toLowerCase)

        val scenesCode = actCode.split("Scene ").toList

        if (scenesCode.length < 2) {
            throw new IllegalArgumentException(s"No scenes in act $id ")
        }
        val scenes = scenesCode.slice(1, scenesCode.size).map(parseScene).toMap

        (id, new Act(id, scenes))
    }

    def parseScene(sceneCode: String): (Int, Scene) = {
        val id = RomanToInt(sceneCode.split(":")(0).trim().toLowerCase)
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

            //println(dialogs(i))
            sceneParts.addAll(parse_statements(dialogs(i)))

            i += 1
            sceneParts.addOne(enterExitBlock)
        }
        (id, new Scene(id, sceneParts.toList))
    }

    def parse_statements(s: String): List[ScenePart] = {
        val sentences = s.split("[A-Z,a-z]*:").filter(s => s.length > 0)
          .map(a => a.replaceAll("\n", " ")).map(a => a.toLowerCase).map(a => a.trim)
          .toList


        val characters = "[A-Z,a-z]*:".r
          .findAllMatchIn(s)
          .map(m => m.group(0).replace(":", "")).map(a => a.toLowerCase.trim)
          .map(s => {
              if (!dictionary.character.contains(s)) throw new IllegalArgumentException(s"ERROR! Character $s is not an Shakespeare character!")
              s
          })
          .toList

        val ret = new ListBuffer[ScenePart]
        for ((c, s) <- characters.map(s => Speaker(s)).zip(sentences.map(s => parse_sentences(s.replace("\r\n", " "))))) {
            ret.addOne(c)
            ret.addOne(s)
        }
        //        println(characters)
        //        print(s)
        ret.toList
    }

    def parse_sentences(str: String): Sentence = {

        val ret = new ListBuffer[Expression]

     // println(str)

        val sentences = str.split("\\.|!|\\?").map(a => a.replaceAll("\n", " ")).map(a => a.toLowerCase).map(a => a.trim).toList
        for (s <- sentences) {
          println(s)
            val printInt = "(open) (.*) (heart)".r.findFirstMatchIn(s)
            val printChar = "(speak) (.*) (.)".r.findFirstMatchIn(s)


            val loadChar = "(open) (.*) (mind)".r.findFirstMatchIn(s)
            val loadInt = "(listen to) (.*) (heart)".r.findFirstMatchIn(s)

            val goto = Set("let us","we shall","we must")

            val pop = "(recall) (.*)".r.findFirstMatchIn(s)

            val push = "(remember) (.*)".r.findFirstMatchIn(s)

            val if_so = "(if so,) (.*)".r.findFirstMatchIn(s)

            val if_not = "(if not,) (.*)".r.findFirstMatchIn(s)

            //println(s)

            val s_split = s.split(" ")

           // println(s_split.slice(0,2).mkString(" "))

            if (goto.contains(s_split.slice(0,2).mkString(" ")))
            {
                //println("3############")
                if ( 5 <= s_split.length )

                    if (s_split(4) == "scene")
                        ret.addOne(GotoS(RomanToInt(s_split(5))))

                    else ret.addOne(GotoA(RomanToInt(s_split(5))))

                else
                    ret.addOne(GotoS(1))
            }

            else if (dictionary.be.contains(s_split(0))){

                val a = s_split.slice(1,s_split.length).mkString(" ")

                val equal_regex = "(.*) (as) (.*) (as) (.*)".r.findFirstMatchIn(a)

                val more_regex = "(.*) (better than) (.*)".r.findFirstMatchIn(a)

                val less_regex = "(.*) (worse than) (.*)".r.findFirstMatchIn(a)

                if (equal_regex.nonEmpty) {

                    ret.addOne(ConditionalBlock(Equal(choose_operation(equal_regex.get.group(1).split(" ").toList)
                        ,choose_operation(equal_regex.get.group(5).split(" ").toList))))
                }

                if (more_regex.nonEmpty) {

                    ret.addOne(ConditionalBlock(More(choose_operation(more_regex.get.group(1).split(" ").toList)
                        ,choose_operation(more_regex.get.group(3).split(" ").toList))))

                }

                if (less_regex.nonEmpty) {

                    ret.addOne(ConditionalBlock(Less(choose_operation(less_regex.get.group(1).split(" ").toList)
                        ,choose_operation(less_regex.get.group(3).split(" ").toList))))

                }

            }
            else if (if_so.nonEmpty) {

                val eval =  if_so.get.group(2).toLowerCase

                println(eval)

                val s = parse_sentences(eval)

                ret.addOne(Then(if_not = false,s.expressions.head))

            }
            else if (if_not.nonEmpty) {

                val eval =  if_not.get.group(2).toLowerCase

                println(eval)


                val s = parse_sentences(eval)

                ret.addOne(Then(if_not = true,s.expressions.head))

            }

            else if (pop.nonEmpty){

                ret.addOne(Pop())
            }
            else if (push.nonEmpty) {

                val possessive =  push.get.group(2).toLowerCase

                if (dictionary.first_person_reflexive.contains(possessive))
                    ret.addOne(Push(true))
                else if (dictionary.second_person_reflexive.contains(possessive))
                    ret.addOne(Push(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possesive word ")

            }

            else
            if (printInt.nonEmpty) {

                val possessive =  printInt.get.group(2).toLowerCase

                if (dictionary.first_person_possessive.contains(possessive))
                    ret.addOne(PrintInt(true))
                else if (dictionary.second_person_possessive.contains(possessive))
                    ret.addOne(PrintInt(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possesive word ")


            }
           else if (printChar.nonEmpty) {

                val possessive = printChar.get.group(2).toLowerCase

                if (dictionary.first_person_possessive.contains(possessive))
                    ret.addOne(PrintChar(true))
                else if (dictionary.second_person_possessive.contains(possessive))
                    ret.addOne(PrintChar(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possessive word ")
            }
            else if (loadChar.nonEmpty){

                val possessive =  loadChar.get.group(2).toLowerCase

                if (dictionary.first_person_possessive.contains(possessive))
                    ret.addOne(LoadChar(true))
                else if (dictionary.second_person_possessive.contains(possessive))
                    ret.addOne(LoadChar(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possesive word ")

            }

            else if (loadInt.nonEmpty) {

                val possessive =  loadInt.get.group(2).toLowerCase

                if (dictionary.first_person_possessive.contains(possessive))
                    ret.addOne(LoadInt(true))
                else if (dictionary.second_person_possessive.contains(possessive))
                    ret.addOne(LoadInt(false))
                else throw new IllegalArgumentException(s"Error, $possessive is not a correct possesive word ")

            }
            else {

                val tokens = s.split(" ")

                val assig = tokens(0)
                //println(assig)

                if (dictionary.first_person.contains(assig)) {

                    ret.addOne(Assigment(speaker = true,get_value(tokens.toList)))

                }

                else if (dictionary.second_person.contains(assig)) {

                    ret.addOne(Assigment(speaker = false,get_value(tokens.toList)))

                }

            }


        }

        Sentence(ret.toList)
    }


    def normal_value(strings: List[String]): Value = {

        if (strings.isEmpty)
            throw new IllegalArgumentException("Error in sentence")

        val word = strings(0)
       //println(word)

        if (dictionary.character.contains(word))
            return SpecifiedCharacterValue(word)

        if (dictionary.second_person.contains(word))
            return CharacterValue(false)

        if (dictionary.first_person.contains(word))
            return CharacterValue(true)

        if (dictionary.first_person_reflexive.contains(word))
            return CharacterValue(true)

        if (dictionary.second_person_reflexive.contains(word))
            return CharacterValue(false)

        // a an or the or my, mine, yours....
        if (dictionary.article.contains(word)
        || dictionary.first_person_possessive.contains(word)
        || dictionary.second_person_possessive.contains(word)
        || dictionary.third_person_possessive.contains(word)){
            return normal_value(strings.slice(1,strings.length))
        }

        if (dictionary.negative_adjective.contains(word) ||
             dictionary.neutral_adjective.contains(word) ||
            dictionary.positive_adjective.contains(word))
                return Adjective(normal_value(strings.slice(1,strings.length)))

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

    def find_split(strings: List[String]): Int = {

        var deep = 1
        var i = 0

        val add_deep = Set("difference","sum","product",
            "quotient","remainder")

        while ( i < strings.length && deep > 0)
        {
            if (strings(i) == "and")
            {
                deep -=1
            }

            if (add_deep.contains(strings(i)))
                deep+=1

            i+=1

        }

        if ( deep != 0)
            throw new IllegalArgumentException("Some error")

        i -=1
        i

    }

    def difference(strings: List[String]): Value = {

        val i = find_split(strings)

        Difference(choose_operation(strings.slice(0,i)),choose_operation(strings.slice(i+1,strings.length)))
    }

    def sum(strings: List[String]): Value = {
        val i = find_split(strings)

        Sum(choose_operation(strings.slice(0,i)),choose_operation(strings.slice(i+1,strings.length)))
    }

    def product(strings: List[String]): Value = {
        val i = find_split(strings)

    Product(choose_operation(strings.slice(0,i)),choose_operation(strings.slice(i+1,strings.length)))
    }

    def quotient(strings: List[String]): Value = {
        val i = find_split(strings)

        Quotient(choose_operation(strings.slice(0,i)),choose_operation(strings.slice(i+1,strings.length)))
    }

    def square(strings: List[String]): Value = {

        Square(choose_operation(strings))

    }

    def square_root(strings: List[String]): Value = {

        SquareRoot(choose_operation(strings))
    }

    def cube(strings: List[String]): Value =
    {

        Cube(choose_operation(strings))

    }

    def twice(strings: List[String]): Value =
    {

        Product(JustValue(2),choose_operation(strings))
    }

    def remainder(strings: List[String]): Value = {

        val i = find_split(strings)

        Remainder(choose_operation(strings.slice(0,i)),choose_operation(strings.slice(i+1,strings.length)))
    }


    def choose_operation(strings: List[String]): Value =
    {

        strings match {

            case "the" :: tail =>
            tail match {

                case "difference" :: "between" :: tail1 => difference(tail1)
                case "sum" :: "of" :: tail1 => sum(tail1)
                case "product" :: "of" :: tail1 =>  product(tail1)
                case "quotient" :: "between" :: tail1 =>  quotient(tail1)
                case "remainder" :: "of" :: "the" :: "quotient" :: "between" :: tail1 => remainder(tail1)
                case "square" :: "of" :: tail1 => square(tail1)
                case "square" :: "root" :: "of" :: tail1 => square_root(tail1)
                case "cube" :: "of" :: tail1 => cube(tail1)
                case "twice" :: tail1 => twice(tail1)
                case tail1 =>  normal_value(tail1)
            }

            case tail =>
            tail match {

                case "difference" :: "between" :: tail1 => difference(tail1)
                case "sum" :: "of" :: tail1 => sum(tail1)
                case "product" :: "of" :: tail1 =>  product(tail1)
                case "quotient" :: "between" :: tail1 =>  quotient(tail1)
                case "remainder" :: "of" :: "the" :: "quotient" :: "between" :: tail1 => remainder(tail1)
                case "square" :: "of" :: tail1 => square(tail1)
                case "square" :: "root" :: "of" :: tail1 => square_root(tail1)
                case "cube" :: "of" :: tail1 => cube(tail1)
                case "twice" :: tail1 => twice(tail1)
                case tail1 =>  normal_value(tail1)
            }

        }

    }

/*
    def choose_operation(words: List[String]): Value = {
        val line = words.mkString(" ")

        words match {
            case "the" :: "difference" :: "between" :: tail =>
                val regex = "(the difference between )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get

                 Difference(
                    choose_operation(regMatch.group(2).split(" ").toList),
                    choose_operation(regMatch.group(3).split(" ").toList))
            case "the" :: "sum" :: "of" :: tail =>
                val regex = "(the sum of )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 Sum(
                    choose_operation(regMatch.group(2).split(" ").toList),
                    choose_operation(regMatch.group(3).split(" ").toList))
            case "the" :: "product" :: "of" :: tail =>
                val regex = "(the product of )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get

                 Product(
                    choose_operation(regMatch.group(2).split(" ").toList),
                    choose_operation(regMatch.group(3).split(" ").toList))

            case "the" :: "cube" :: "of" :: tail =>
                val regex = "(the cube of )(.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 Cube(
                    choose_operation(regMatch.group(2).split(" ").toList))

            case "twice" :: tail =>
                val regex = "(twice )(.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 Product(
                    choose_operation(regMatch.group(2).split(" ").toList),JustValue(2))


            case "the" :: "quotient" :: "between" :: tail =>
                val regex = "(the quotient between )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 Quotient(
                    choose_operation(regMatch.group(2).split(" ").toList),
                    choose_operation(regMatch.group(3).split(" ").toList))
            case "the" :: "square" :: "of" :: tail =>
                val regex = "(the square of )(.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 Square(
                    choose_operation(regMatch.group(2).split(" ").toList))
            case "the" :: "square" :: "root" :: "of" :: tail =>
                val regex = "(the square root of )(.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 SquareRoot(
                    choose_operation(regMatch.group(2).split(" ").toList))
            case "the" :: "remainder" :: "between" :: tail =>
                val regex = "(the remainder between )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                 Remainder(
                    choose_operation(regMatch.group(2).split(" ").toList),
                    choose_operation(regMatch.group(3).split(" ").toList))
            case constant =>  normal_value(words)
        }
    }
*/

    def get_value(strings: List[String]) : Value = {


        if (dictionary.be.contains(strings(1)))
            {
                strings match {

                    case _ :: _ :: tail =>
                    tail match {

                        case "as" :: _  ::"as" :: tail1 => choose_operation(tail1)
                        case tail1 => normal_value(tail1)
                    }
                }
            }
        else {

            strings match {
                case _ :: tail =>
                tail match {

                    case "as" :: _  ::"as" :: tail1 => choose_operation(tail1)
                    case tail1 => normal_value(tail1)

                }
            }
        }
    }


    def RomanToInt(roman: String): Int = {
        if (!roman.matches("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$")) {
            throw new IllegalArgumentException(s"$roman is not roman numeral")
        }


        val roman_numerals_map: Map[Char, Int] = Map('m' -> 1000, 'd' -> 500,
            'c' -> 100, 'l' -> 50, 'x' -> 10, 'v' -> 5, 'i' -> 1)

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
