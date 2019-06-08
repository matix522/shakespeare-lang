object Main {
  def loadFile(filename: String ):String = {
    val source = scala.io.Source.fromFile("file.txt")
    val lines = try source.mkString finally source.close()
    lines
  }
  def main(args: Array[String]): Unit = {
//    val fileName : String = args(0)
//    val sourceCode = loadFile(fileName)

val sourceCode = "Romeo, a young man with a remarkable patience.\nJuliet, a likewise young woman of remarkable grace.\nOphelia, a remarkable woman much in dispute with Hamlet.\nHamlet, the flatterer of Andersen Insulting A/S.\n\n\n                   Act I: Hamlet's insults and flattery.\n\n                   Scene I: The insulting of Romeo.\n\n[Enter Hamlet and Romeo]\n\nHamlet:\nYou lying stupid fatherless big smelly half-witted coward! You are as\nstupid as the difference between a handsome rich brave hero and thyself!\nSpeak your mind!\n\nYou are as brave as the sum of your fat little stuffed misused dusty\nold rotten codpiece and a beautiful fair warm peaceful sunny summer's\nday. You are as healthy as the difference between the sum of the\nsweetest reddest rose and my father and yourself! Speak your mind!\n\nYou are as cowardly as the sum of yourself and the difference\nbetween a big mighty proud kingdom and a horse. Speak your mind.\n\nSpeak your mind!\n\n[Exit Romeo]\n\n                   Scene II: The praising of Juliet.\n\n[Enter Juliet]\n\nHamlet:\nThou art as sweet as the sum of the sum of Romeo and his horse and his\nblack cat! Speak thy mind!\n\n[Exit Juliet]\n\n                   Scene III: The praising of Ophelia.\n\n[Enter Ophelia]\n\nHamlet:\nThou art as lovely as the product of a large rural town and my amazing\nbottomless embroidered purse. Speak thy mind!\n\nThou art as loving as the product of the bluest clearest sweetest sky\nand the sum of a squirrel and a white horse. Thou art as beautiful as\nthe difference between Juliet and thyself. Speak thy mind!\n\n[Exeunt Ophelia and Hamlet]\n\n\n                   Act II: Behind Hamlet's back.\n\n                   Scene I: Romeo and Juliet's conversation.\n\n[Enter Romeo and Juliet]\n\nRomeo:\nSpeak your mind. You are as worried as the sum of yourself and the\ndifference between my small smooth hamster and my nose. Speak your\nmind!\n\nJuliet:\nSpeak YOUR mind! You are as bad as Hamlet! You are as small as the\ndifference between the square of the difference between my little pony\nand your big hairy hound and the cube of your sorry little\ncodpiece. Speak your mind!\n\n[Exit Romeo]\n\n                   Scene II: Juliet and Ophelia's conversation.\n\n[Enter Ophelia]\n\nJuliet:\nThou art as good as the quotient between Romeo and the sum of a small\nfurry animal and a leech. Speak your mind!\n\nOphelia:\nThou art as disgusting as the quotient between Romeo and twice the\ndifference between a mistletoe and an oozing infected blister! Speak\nyour mind!\n\n[Exeunt]"
    var parser = new Parser(sourceCode)
    parser.parse()
  }
}
