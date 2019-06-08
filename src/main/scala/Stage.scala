class Stage {
    var speaker: Option[Character] = None
    var listener: Option[Character] = None

    def enter(character: Character) = {
        if (speaker.isEmpty) speaker = Some(character)
        else if (listener.isEmpty) listener = Some(character)
        else throw new RuntimeException("There are already 2 characters on stage.")
    }

    def enter(character1: Character, character2: Character) = {
        if (speaker.isEmpty) speaker = Some(character1)
        else throw new RuntimeException("There are already 2 characters on stage.")
        if (listener.isEmpty) listener = Some(character2)
        else throw new RuntimeException("There are already 2 characters on stage.")
    }

    def exit(character: Character): Unit = {
        speaker match {
            case Some(`character`) => {
                speaker = None;
                return
            }
        }
        listener match {
            case Some(`character`) => {
                listener = None;
                return
            }
        }
        throw new RuntimeException(s"There is no $character on the scene.")
    }

    def exeunt() = {
        speaker = None
        listener = None
    }

    def exeunt(character1: Character, character2: Character): Unit = {
        speaker match {
            case Some(`character1`) => speaker = None
            case Some(`character2`) => speaker = None

            case _ => throw new RuntimeException(s"There are no $character1 or $character2 on the scene.")

        }
        listener match {
            case Some(`character1`) => listener = None
            case Some(`character2`) => listener = None
            case _ => throw new RuntimeException(s"There are no $character1 or $character2 on the scene.")
        }
    }

    def changeSpeaker(character: Character) = {
        listener match {
            case Some(`character`) => listener = speaker
            case _ => throw new RuntimeException(s"$character is not listener on scene.")
        }
        speaker = Some(character)
    }

    def isOnStage(character: Character): Boolean = {
        listener match {
            case Some(`character`) => return true
        }
        speaker match {
            case Some(`character`) => return true
        }
        false
    }


    def getSpeaker(): Character = {
        speaker match {
            case Some(s) => s
            case None => throw new RuntimeException("There is no speaker.")
        }
    }

    def getListener(): Character = {
        listener match {
            case Some(s) => s
            case None => throw new RuntimeException("There is no listener.")
        }
    }
}