import scala.io.Source

object Main {
    def loadFile(filename: String): String = {
        val source = scala.io.Source.fromFile(filename)
        val lines = try source.mkString finally source.close()
        lines
    }

    def main(args: Array[String]): Unit = {

        var dictionary = new Dictionary;

        val sourceCode = loadFile(args(0))

        var parser = new Parser(sourceCode, dictionary)
        parser.parse()
    }
}
