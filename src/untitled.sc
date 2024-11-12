import scala.util.Random

def generateLength(min: Int, max: Int): Int = {
  Random.between(min, max + 1)
}

def generateChar(charType: String): Char = {
  charType match {
    case "upper" => (Random.nextInt(26) + 'A').toChar
    case "lower" => (Random.nextInt(26) + 'a').toChar
    case "int" => (Random.nextInt(10) + '0').toChar
    case "special" => Random.between(33,64).toChar
  }
}

def makeList(length:Int, charType: String): List[Char] = {
  LazyList.continually(generateChar(charType))
    .take(length)
    .toList
}

def generateRandomString(numbers: Boolean, special: Boolean, upper: Boolean): String = {

  val generatedString = {
    if (numbers && special && upper) {
      val maxLength = generateLength(4, 16)
      val lower = makeList(generateLength(1, maxLength - 3), "upper")
      val number = makeList((maxLength - 2)  - lower.length, "lower")
      val special = makeList(maxLength-1 - (lower.length + number.length), "special")
      val upper = makeList(maxLength - (lower.length + number.length + special.length), upper = true)
      Random.shuffle(lower ++ number ++ special ++ upper).mkString
    }
    else if (numbers && special) {
      val maxLength = generateLength(3, 16)
      val lower = generateChar(generateLength(1, maxLength - 2), upper = false)
      val number = generateChar((maxLength-1) - lower.length)
      val special = generateChar(maxLength - (lower.length + number.length))
      Random.shuffle(lower ++ number ++ special).mkString
    }
    else if(numbers) {
      val maxLength = generateLength(2, 16)
      val lower = generateChar(generateLength(1, maxLength - 1), upper = false)
      val intChars = generateChar(maxLength - lower.length)
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
}

println(makeList(10, "special").mkString)
