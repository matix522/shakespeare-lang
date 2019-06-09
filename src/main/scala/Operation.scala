object Operation {
    def parseIntoAtoms(words: List[String]): List[String] = {
        val line = words.mkString(" ")
        val regex = "(?=(the|and|a|an)\\s(.*?)\\s(the|and|a|an))".r
        val expressions = regex.findAllIn(line).matchData.map(
            m => m.group(2)
        ).toList
        val lastRegex = "(?=((and)(.*?)$))".r
        val lastExpressions = lastRegex.findAllIn(line).matchData.reduceLeft((a, b) => b).group(3).trim
        val result = expressions.appended(lastExpressions.trim)

        result
    }

    def parseOperations(words: List[String]): Value = {
        val atoms = parseIntoAtoms(words)

        val (value, leftovers) = parseOperation(atoms)
        if (leftovers.length > 0)
            throw new IllegalArgumentException("Incorrect value definition")
        value
    }

    def parseOperation(atoms: List[String]): (Value, List[String]) = {
        atoms match {
            case "difference between" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                val (value2, atoms2) = parseOperation(atoms1)
                (Difference(value1, value2), atoms2)
            case "sum of" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                val (value2, atoms2) = parseOperation(atoms1)
                (Sum(value1, value2), atoms2)
            case "product of" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                val (value2, atoms2) = parseOperation(atoms1)
                (Product(value1, value2), atoms2)
            case "cube of" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                (Cube(value1), atoms1)
            case "twice" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                (Product(JustValue(2), value1), atoms1)
            case "quotient between" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                val (value2, atoms2) = parseOperation(atoms1)
                (Quotient(value1, value2), atoms2)
            case "square of" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                (Square(value1), atoms1)
            case "square root of" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                (SquareRoot(value1), atoms1)
            case "remainder between" :: tail =>
                val (value1, atoms1) = parseOperation(tail)
                val (value2, atoms2) = parseOperation(atoms1)
                (Difference(value1, value2), atoms2)
            case a :: tail => (normal_value(a), tail)
        }
    }
}
