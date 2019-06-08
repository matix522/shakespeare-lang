class Act (val id : String, val scenes: Map[Int,Scene]) {
    override def toString = s"Act(\nid=$id, \nscenes=$scenes\n)\n"
}
