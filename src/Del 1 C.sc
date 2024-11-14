
import scala.util.Random

object RandomStringGenerator {

  def generateLength(min: Int, max: Int): Int = {
    Random.between(min, max + 1)
  }

  def generateLetters(length: Int, upper:Boolean ): List[Char] = {
    LazyList.continually(Random.nextInt(26) + 'a')
      .map(c => {
        val char = c.toChar
        if (upper) char.toUpper else char
      })
      .take(length)
      .toList
  }

  def generateNumbers(length: Int): List[Char] = {
    LazyList.continually(Random.nextInt(10) + '0')
      .map(_.toChar)
      .take(length)
      .toList
  }

  def generateSpecial(length: Int): List[Char] = {
    LazyList.continually(Random.between(33,64))
      .map(_.toChar)
      .take(length)
      .toList
  }

  def generateRandomString(numbers: Boolean, special: Boolean, upper: Boolean): String = {

    val generatedString = {
      if (numbers && special && upper) {
        val maxLength = generateLength(4, 16)
        val lower = generateLetters(generateLength(1, maxLength - 3), upper = false)
        val number = generateNumbers((maxLength-2) - lower.length)
        val special = generateSpecial(maxLength-1 - (lower.length + number.length))
        val upper = generateLetters(maxLength - (lower.length + number.length + special.length), upper = true)
        Random.shuffle(lower ++ number ++ special ++ upper).mkString
      }
      else if (numbers && special) {
        val maxLength = generateLength(3, 16)
        val lower = generateLetters(generateLength(1, maxLength - 2), upper = false)
        val number = generateNumbers((maxLength-1) - lower.length)
        val special = generateSpecial(maxLength - (lower.length + number.length))
        Random.shuffle(lower ++ number ++ special).mkString
      }
      else if(numbers) {
        val maxLength = generateLength(2, 16)
        val lower = generateLetters(generateLength(1, maxLength - 1), upper = false)
        val intChars = generateNumbers(maxLength - lower.length)
        Random.shuffle(lower ++ intChars).mkString
      }
      else {
        val maxLength = generateLength(1, 16)
        generateLetters(maxLength, upper = false).mkString
      }
    }
    generatedString
  }

  def generateStringList(n:Boolean, s:Boolean, u:Boolean): List[String] = {
    LazyList.continually(generateRandomString(n, s, u))
      .distinct
      .take(10)
      .toList
      .sorted
  }
}

println(s"${RandomStringGenerator.generateStringList(n = true, s = true, u = true).mkString(" , ")}")




