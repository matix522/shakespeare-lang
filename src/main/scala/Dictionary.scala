import scala.collection.mutable
import scala.io.Source

class Dictionary ()
{

  val directory : String = "./words/"
  val file_extension = ".wordlist"

  var  article: mutable.Set[String] = mutable.Set[String]()
  var be: mutable.Set[String] = mutable.Set[String]()
  var character: mutable.Set[String] = mutable.Set[String]()
  var first_person: mutable.Set[String] = mutable.Set[String]()
  var first_person_possessive: mutable.Set[String] = mutable.Set[String]()
  var first_person_reflexive: mutable.Set[String] = mutable.Set[String]()
  var negative_adjective: mutable.Set[String] = mutable.Set[String]()
  var negative_comparative: mutable.Set[String] = mutable.Set[String]()
  var negative_noun: mutable.Set[String] = mutable.Set[String]()
  var neutral_adjective: mutable.Set[String] = mutable.Set[String]()
  var neutral_noun: mutable.Set[String] = mutable.Set[String]()
  var nothing: mutable.Set[String] = mutable.Set[String]()
  var positive_adjective: mutable.Set[String] = mutable.Set[String]()
  var positive_comparative: mutable.Set[String] = mutable.Set[String]()
  var positive_noun: mutable.Set[String] = mutable.Set[String]()
  var second_person: mutable.Set[String] = mutable.Set[String]()
  var second_person_possessive: mutable.Set[String] = mutable.Set[String]()
  var second_person_reflexive: mutable.Set[String] = mutable.Set[String]()
  var third_person_possessive: mutable.Set[String] = mutable.Set[String]()

  val roman_numerals_map : Map[String,Int] = Map("M" -> 1000, "D" -> 500,
  "C" -> 100, "L" -> 50, "X" -> 10, "V" -> 5, "I" -> 1)


  var file : String = directory + "article" + file_extension

  for (line <- Source.fromFile(file).getLines){

    article.add(line.replace("\n",""))
  }


  file = directory + "be" + file_extension
  for (line <- Source.fromFile(file).getLines){

    be.add(line.replace("\n",""))
  }


  file = directory + "character" + file_extension
  for (line <- Source.fromFile(file).getLines){

    character.add(line.replace("\n",""))
  }


  file = directory + "first_person" + file_extension
  for (line <- Source.fromFile(file).getLines){

    first_person.add(line.replace("\n",""))
  }

  file = directory + "first_person_possessive" + file_extension
  for (line <- Source.fromFile(file).getLines){

    first_person_possessive.add(line.replace("\n",""))
  }

  file = directory + "first_person_reflexive" + file_extension
  for (line <- Source.fromFile(file).getLines){

    first_person_reflexive.add(line.replace("\n",""))
  }

  file = directory + "negative_adjective" + file_extension
  for (line <- Source.fromFile(file).getLines){

    negative_adjective.add(line.replace("\n",""))
  }

  file = directory + "negative_comparative" + file_extension
  for (line <- Source.fromFile(file).getLines){

    negative_comparative.add(line.replace("\n",""))
  }

  file = directory + "negative_noun" + file_extension
  for (line <- Source.fromFile(file).getLines){

    negative_noun.add(line.replace("\n",""))
  }

  file = directory + "neutral_adjective" + file_extension
  for (line <- Source.fromFile(file).getLines){

    neutral_adjective.add(line.replace("\n",""))
  }

  file = directory + "neutral_noun" + file_extension
  for (line <- Source.fromFile(file).getLines){

    neutral_noun.add(line.replace("\n",""))
  }

  file = directory + "nothing" + file_extension
  for (line <- Source.fromFile(file).getLines){

    nothing.add(line.replace("\n",""))
  }

  file = directory + "positive_adjective" + file_extension
  for (line <- Source.fromFile(file).getLines){

    positive_adjective.add(line.replace("\n",""))
  }

  file = directory + "positive_comparative" + file_extension
  for (line <- Source.fromFile(file).getLines){

    positive_comparative.add(line.replace("\n",""))
  }

  file = directory + "positive_noun" + file_extension
  for (line <- Source.fromFile(file).getLines){

    positive_noun.add(line.replace("\n",""))
  }

  file = directory + "second_person" + file_extension
  for (line <- Source.fromFile(file).getLines){

    second_person.add(line.replace("\n",""))
  }

  file = directory + "second_person_possessive" + file_extension
  for (line <- Source.fromFile(file).getLines){

    second_person_possessive.add(line.replace("\n",""))
  }

  file = directory + "second_person_reflexive" + file_extension
  for (line <- Source.fromFile(file).getLines){

    second_person_reflexive.add(line.replace("\n",""))
  }

  file = directory + "third_person_possessive" + file_extension
  for (line <- Source.fromFile(file).getLines){

    third_person_possessive.add(line.replace("\n",""))
  }







}