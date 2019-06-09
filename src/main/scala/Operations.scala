import scala.util.matching.Regex

class Operations {
    def parseOperations(words: List[String]): Value = {
        val line = words.mkString(" ")

        words match {
            case "the" :: "difference" :: "between" :: tail =>
                val regex = "(the difference between )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return Difference(
                    parseOperations(regMatch.group(2).split(" ").toList),
                    parseOperations(regMatch.group(3).split(" ").toList))
            case "the" :: "sum" :: "of" :: tail =>
                val regex = "(the sum of )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return Sum(
                    parseOperations(regMatch.group(2).split(" ").toList),
                    parseOperations(regMatch.group(3).split(" ").toList))
            case "the" :: "product" :: "of" :: tail =>
                val regex = "(the product of )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return Product(
                    parseOperations(regMatch.group(2).split(" ").toList),
                    parseOperations(regMatch.group(3).split(" ").toList))
            case "the" :: "quotient" :: "between" :: tail =>
                val regex = "(the quotient between )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return Quotient(
                    parseOperations(regMatch.group(2).split(" ").toList),
                    parseOperations(regMatch.group(3).split(" ").toList))
            case "the" :: "square" :: "of" :: tail =>
                val regex = "(the square of )(.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return Square(
                    parseOperations(regMatch.group(2).split(" ").toList))
            case "the" :: "square" :: "root" :: "of" :: tail =>
                val regex = "(the square root of )(.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return SquareRoot(
                    parseOperations(regMatch.group(2).split(" ").toList))
            case "the" :: "remainder" :: "between" :: tail =>
                val regex = "(the remainder between )(.*) and (.*)".r
                val regMatch = regex.findFirstMatchIn(line).get
                return Remainder(
                    parseOperations(regMatch.group(2).split(" ").toList),
                    parseOperations(regMatch.group(3).split(" ").toList))
            case constant => return parseValue(words)
        }
    }

    def parseValue(words: List[String]): Value = {
        JustValue(0)
    }
}
