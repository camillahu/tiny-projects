import scala.util.Random

object ListGenerator {

  def generateLength(min: Int, max: Int): Int = Random.between(min, max + 1)

  def stringGenerator():String = {
    LazyList.continually(Random.nextPrintableChar())
    .take(generateLength(1, 10))
    .mkString
  }

  def stringListGenerator(maxLength:Int): List[String] = {
    LazyList.continually(stringGenerator())
      .take(generateLength(1, maxLength))
      .distinct
      .toList
  }

  def defaultStringGenerator(): List[String] = {
    val defaultString: String = "AbCDeEFg"
    val newList:List[String] = defaultString +: stringListGenerator(7)
    Random.shuffle(newList)
  }

  def motherList(): List[List[String]] = {
    val randomList = LazyList.continually(stringListGenerator(8))
      .take(99999)
      .toList

    val listWithDefault= defaultStringGenerator() +: randomList
    Random.shuffle(listWithDefault)
  }
}
println(ListGenerator.motherList().map( innerList => innerList.mkString(", ")).take(100).mkString("\n"))


