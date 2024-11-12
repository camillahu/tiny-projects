import scala.util.Random

object Utils {

  def generateLength(min: Int, max: Int): Int = {
    Random.between(min, max + 1)
  }

  def generateChar(charType: String, length:Int): List[Char] = {

    val generator= charType match {
      case "upper" => (Random.nextInt(26) + 'A').toChar
      case "lower" => (Random.nextInt(26) + 'a').toChar
      case "singleDigitNumber" => (Random.nextInt(10) + '0').toChar
      case "singleDigitNumber" => Random.between(33,64).toChar

    }
    LazyList.continually(generator)
      .take(length)
      .toList
  }

//  case "randomDigitNumber" => (Random.nextInt(generateLength()))
//  case "doubleDigitDecimal" => BigDecimal(Random.nextDouble()).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString

//  def generateLetters(length: Int, upper:Boolean ): List[Char] = {
//    LazyList.continually(Random.nextInt(26) + 'a')
//      .map(c => {
//        val char = c.toChar
//        if (upper) char.toUpper else char
//      })
//      .take(length)
//      .toList
//  }

//  def generateNumbers(length: Int): List[Char] = {
//    LazyList.continually(Random.nextInt(10) + '0')
//      .map(_.toChar)
//      .take(length)
//      .toList
//  }

//  def generateDoubles(length: Int): List[Char] = {
//    LazyList.continually(BigDecimal(Random.nextDouble()).setScale(2, BigDecimal.RoundingMode.HALF_UP))
//      .map(_.toChar)
//      .take(length)
//      .toList
//  }

  def generateSpecial(length: Int): List[Char] = {
    LazyList.continually(Random.between(33,64))
      .map(_.toChar)
      .take(length)
      .toList
  }

  def randomStringGenerator():String = {
    LazyList.continually(Random.nextPrintableChar())
      .take(generateLength(1, 10))
      .mkString
  }
}