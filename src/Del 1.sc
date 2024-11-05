import scala.collection.mutable
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, Set}

def PrintArraySamples(choice: Int): Unit = {

  //      def generateArrayNoDup(start: Int, end: Int): Array[Int] = {
  //        val numbers:Seq[Int] = start until end
  //        val shuffled:Array[Int] = Random.shuffle(numbers).toArray
  //       shuffled
  //      }
  //

  def generateArray(min: Int, max: Int, duplicate:Boolean): Array[Int] = {
    val count:Int = 1000000

    val numbers = if (duplicate) {
      ArrayBuffer[Int]() //ArrayBuffer gir mulighet for duplikater
    } else {
      mutable.Set[Int]() //Set gir unike tall
    }

    while (numbers.size < count) {
      //så lenge numbers er mindre enn count, så genereres nye random tall mellom en min og max-verdi.
      //randomnum settes inn i numbers.
      //denne vil fungere både på iterables som kan ha duplikater og ikke.
      val randomNum = Random.nextInt(max - min) + min
      numbers += randomNum
    }

    //returnerer resultatet som et array
    val result: Array[Int] = numbers.toArray
    result
    }

  // pattern matching for valg (muligens implementere brukervalg)
  choice match {
    case 1 => println(generateArray(min = 0, max= 200000000, duplicate= false).take(10).mkString(", "))
    case 2 => println(generateArray(min = -5000000, max = 10000000, duplicate = false).take(10).mkString(", "))
    case 3 => println(generateArray(min = -500000, max = 0, duplicate = true).take(10).mkString(", "))
  }
}

PrintArraySamples(2)
















