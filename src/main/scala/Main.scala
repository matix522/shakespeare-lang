import scala.io.Source

object Main {
    def loadFile(filename: String): String = {
        val source = scala.io.Source.fromFile(filename)
        val lines = try source.mkString finally source.close()
        lines
    }

    def main(args: Array[String]): Unit = {

        val dictionary = new Dictionary;

        val sourceCode = loadFile(args(0))
        if(args.length > 1) {
            val parser = new Parser(sourceCode, dictionary)
            val (characters, play) = parser.parse(args(1).toBoolean)
            val interpreter = new Interpreter(characters,play)
            interpreter.execute(args(1).toBoolean)
        }else {
            val parser = new Parser(sourceCode, dictionary)
            val (characters, play) = parser.parse()
            val interpreter = new Interpreter(characters,play)
            interpreter.execute()
        }

        println("")
    }
}
