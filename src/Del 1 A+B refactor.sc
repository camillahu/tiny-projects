import scala.util.Random

object AB {
  private val random = new Random()  //lager et instans av random, så vi slipper å lage nytt hele tiden.

  def generateInt(min: Int, max: Int): Int = min + random.nextInt(max- min + 1)

  def generateDouble(min: Int, max: Int): Double = {
    val intPart = generateInt(min, max)
    val decimalPart = random.nextDouble()
    intPart + decimalPart
  }

  def generateIntArray(min: Int, max: Int, allowDuplicates: Boolean, count:Int = 1000000): Array[Int] = {
    if (allowDuplicates) Array.fill(count)(generateInt(min, max))
    else  {
      val range = max-min + 1
      if(count > range) throw new IllegalArgumentException(s"Cannot generate $count unique numbers in range $min to $max")
      random.shuffle((min to max).toVector).take(count).map(i => generateInt(i, i)).toArray
    }
  }

  def generateDoubleArray(min: Int, max: Int, allowDuplicates: Boolean, count:Int = 1000000): Array[Double] = {
    if (allowDuplicates) Array.fill(count)(generateDouble(min, max))
    else  {
      val range = max-min + 1
      if(count > range) throw new IllegalArgumentException(s"Cannot generate $count unique numbers in range $min to $max")
      random.shuffle((min to max).toVector).take(count).map(i => generateDouble(i, i)).toArray
    }
  }

  def arrayToPrint(choice: String): String = {
    choice match  {
      case "1"=> generateIntArray(0, 2000000, allowDuplicates = false).mkString(", ")
      case "2"=> generateIntArray(-500000, 1000000,allowDuplicates = false).mkString(", ")
      case "3"=>  generateIntArray(-500000, 1000000,allowDuplicates = true).mkString(", ")
      case "4"=> generateDoubleArray(0, 2000000, allowDuplicates = false).mkString(", ")
      case "5"=> generateDoubleArray(-5000, 10000, allowDuplicates = false).mkString(", ")
      case "6"=>generateDoubleArray(-5000, 10000, allowDuplicates = true).mkString(", ")
    }
  }
}

println(AB.arrayToPrint("2"))



