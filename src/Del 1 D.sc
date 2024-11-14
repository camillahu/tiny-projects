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

  lazy val motherList: List[List[String]] = {
    val randomList = LazyList.continually(stringListGenerator(8))
      .take(999)
      .toList

    val listWithDefault= defaultStringGenerator() +: randomList
    Random.shuffle(listWithDefault)
  }

  def findIndexOf(defaultString:String = "AbCDeEFg", motherList: List[List[String]] = motherList): Option[(Int, Int)] = {
    val index = motherList.zipWithIndex.collectFirst {
      case (innerList, i) if innerList.contains(defaultString) =>
        (i, innerList.indexOf(defaultString))
    }
    index
  }

  def printIndexes():Unit = {
    findIndexOf() match {
      case Some((outerIndex, innerIndex)) => println(s"outer index: $outerIndex, inner index: $innerIndex")
      case None => println("Element not found")
    }
  }
}
println(ListGenerator.motherList.map( innerList => innerList.mkString(", ")).mkString("\n"))
ListGenerator.printIndexes()



