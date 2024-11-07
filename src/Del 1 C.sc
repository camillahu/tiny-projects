import scala.util.Random

object RandomStringGenerator {

  def generateLength(): Int = {
    Random.between(1, 17)
  }

  def generateLowerCaseString(): String = {
    LazyList.continually(Random.nextPrintableChar())
      .filter(_.isLetter)
      .map(_.toLower)
      .take(generateLength())
      .mkString
  }

  def generateStringList: List[String] = {
    LazyList.continually(generateLowerCaseString())
      .distinct
      .take(10)
      .toList
  }
}

println(s"${RandomStringGenerator.generateStringList.mkString(", ")}")









