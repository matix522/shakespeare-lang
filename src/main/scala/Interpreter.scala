class Interpreter(var character: Map[String,Character], val acts: List[Act]) {
    var stage = new Stage
    def execute() = {
        var actNumber = 0


        while (actNumber < acts.length) {
            var sceneNumber = 0;
            val act = acts(actNumber)
            while (sceneNumber < act.scenes.length) {
                var scene = act.scenes(sceneNumber)
                for(scenePart <- scene.sceneParts){
                    scenePart match {
                        case Enter(first,None) => stage.enter(character(first))
                        case Enter(first,second) => stage.enter(character(first),character(second))
                        case Exeunt(None,None) => stage.exeunt()
                        case Exeunt(first,second) => stage.exeunt(character(first),character(second))
                        case Exit(first) => stage.exit(character(first))
                        case Speaker(first) => stage.changeSpeaker(character(first))
                        case Sentence(expressions) => doExpressions(expressions)
                    }
                }
                sceneNumber += 1
            }
            actNumber += 1;
        }
    }
    def doExpressions(expressions: List[Expression]) = {

    }
}
