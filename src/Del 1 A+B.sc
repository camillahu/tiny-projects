import scala.collection.mutable
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, Set}


def A(choice: Int): Unit = {

  def generateArray(min: Int, max: Int, duplicate: Boolean): Array[Int] = {
    val count: Int = 1000000

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
    case 1 => println(generateArray(min = 0, max = 200000000, duplicate = false).take(10).mkString(", "))
    case 2 => println(generateArray(min = -5000000, max = 10000000, duplicate = false).take(10).mkString(", "))
    case 3 => println(generateArray(min = -5000000, max = 10000000, duplicate = true).take(10).mkString(", "))
  }
}



def B(choice: Int): Unit = {

  def generateArray(min: Int, max: Int, duplicate: Boolean): Array[Double] = {
    val count: Int = 1000000

    val numbers = if (duplicate) {
      ArrayBuffer[Double]()
    } else {
      mutable.Set[Double]()
    }

    while (numbers.size < count) {
      val randomInt = Random.between(min,max)
      val randomDouble = BigDecimal(Random.nextDouble).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      val randomNum = randomInt + randomDouble
      numbers += randomNum;
    }
    val result: Array[Double] = numbers.toArray
    result
  }

  choice match {
    case 1 => println(generateArray(min = 0, max = 10000, duplicate = false).take(10).mkString(", "))
    case 2 => println(generateArray(min = -5000, max = 10000, duplicate = false).take(10).mkString(", "))
    case 3 => println(generateArray(min = -5000, max = 10000, duplicate = true).take(10).mkString(", "))
  }
}


//-----------------------------------------------

//(ghatGpt-hjelp)

//def A(choice: Int): Unit = {
//
//  def generateArray(min: Int, max: Int, duplicate: Boolean): Array[Int] = {
//
//    val count: Int = 1000
//
//    if (duplicate) {
//      LazyList.fill(count)(Random.between(min, max)).toArray
//    } else {
//      LazyList
//        .continually(Random.between(min, max))
//        .distinct
//        .take(count)
//        .toArray
//    }
//  }
//
//  choice match {
//    case 1 => println(generateArray(min = 0, max = 10000, duplicate = false).take(10).mkString(", "))
//    case 2 => println(generateArray(min = -5000, max = 5000, duplicate = false).take(10).mkString(", "))
//    case 3 => println(generateArray(min = -5000, max = 5000, duplicate = true).take(10).mkString(", "))
//  }
//}

//-----------------------------------------------

//Refactor 2 (perplexity-hjelp):

//object RandomNumGenerator {
//
//  def generateNums[T](min: Int, max: Int, datatype: String, allowDuplicates: Boolean): Vector[T] = {
//    val count = 1000
//
//    val generator: () => T = datatype match {
//      case "int" => () => Random.between(min, max).asInstanceOf[T]
//      case "double" => () =>
//        BigDecimal(Random.between(min, max).toDouble + Random.nextDouble())
//          .setScale(2, BigDecimal.RoundingMode.HALF_UP).asInstanceOf[T]
//      case _ => throw new IllegalArgumentException("Unsupported datatype")
//    }
//    if (allowDuplicates) {
//      LazyList.fill(count)(generator()).toVector
//    } else {
//      LazyList
//        .continually(generator())
//        .distinct
//        .take(count)
//        .toVector
//    }
//  }
//
//  def printFirstTen[T](numbers: Vector[T]): Unit = {
//    println(numbers.take(10).mkString(", "))
//  }
//
//  def generateAndPrint(choice: Int): Unit = {
//    val result = choice match {
//      case 1 => generateNums(min = 0, max = 10000, datatype = "int", allowDuplicates = false)
//      case 2 => generateNums(min = -5000, max = 5000, datatype = "int", allowDuplicates = false)
//      case 3 => generateNums(min = -5000, max = 5000, datatype = "int", allowDuplicates = true)
//      case 4 => generateNums(min = 0, max = 10000, datatype = "double", allowDuplicates = false)
//      case 5 => generateNums(min = -5000, max = 5000, datatype = "double", allowDuplicates = false)
//      case 6 => generateNums(min = -5000, max = 5000, datatype = "double", allowDuplicates = true)
//      case _ => Vector.empty[Double]
//    }
//    printFirstTen(result)
//  }
//}
//
//RandomNumGenerator.generateAndPrint(4)

//-----------------------------------------------

//copied code from perplexity when asking about making this even more functional:
//Use pure functions: Make functions pure by avoiding side effects and making them return values instead of performing actions.
//  Leverage pattern matching more: Use it for type-safe alternatives to if-else statements.
//  Use Options instead of throwing exceptions: This makes error handling more functional.
//Use higher-order functions: Embrace functions that take other functions as parameters or return functions.
//  Utilize more immutable data structures: Prefer immutable collections over mutable ones.
//Use type classes for better polymorphism: This can make your code more flexible and extensible.

object RandomNumGenerator {

  sealed trait NumberType
  //trait kan kalles for en type interface. Denne er sealed, og kan bare extendes i denne filen.
  //grunnen til at dette brukes her er at i kombinasjon med case objects,
  //begrenser hvor mange mulige verdier kompilatoren kan sjekke (error handling).
  case object IntType extends NumberType
  case object DoubleType extends NumberType
  //case object er liknende case class, men tar ingen argumenter.
  //har mer funkjsoner enn et vanlig objekt.

  case class GeneratorConfig(min: Int, max: Int, numberType: NumberType, allowDuplicates: Boolean)
  //case class har ekstra funksjoner som gjør det enklere å jobbe med data.
  // her brukes den for å definere hvordan en generator skal se ut, og bruker den senere i koden.
  //på denne måten gjør vi det enklere å bruke flere steder i koden.

  trait NumberGenerator[T] {
    def generate(min: Int, max: Int): T
  }
  //dette kalles en type class. Det er en måte å legge til funksjonalitet til typer uten å endre originalkoden.
  // her defineres en interface for å generere tall (både int og double) for å unngå repetisjon.

  implicit val intGenerator: NumberGenerator[Int] =
    (min: Int, max: Int) => Random.between(min, max)
  //denne returnerer en NumberGenerator som er utvidet til å returnere en int,
  //som genereres med random mellom min og max-parameterne

  implicit val doubleGenerator: NumberGenerator[Double] =
    (min: Int, max: Int) => BigDecimal(Random.between(min, max).toDouble + Random.nextDouble())
      .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  //denne returnerer en NumberGenerator som er utvidet til å returnere en double,
  //som genereres med random mellom min og max-parameterne med to desimaler.

  def generateNums[T](config: GeneratorConfig)(implicit gen: NumberGenerator[T]): Vector[T] = {
    //funksjonen generateNums er generic for å kunne brukes med int og double.
    //parameteret config tar case classen GeneratorConfig som inneholder detaljene for hvordan generatoren skal være.
    //implicit blir brukt i det andre parameteret fordi kompilatoren vil prøve å finne en matchende generator i scopet automatisk.
    val generator = () => gen.generate(config.min, config.max)

    val stream = LazyList.continually(generator())
    val distinctStream = if (config.allowDuplicates) stream else stream.distinct

    distinctStream.take(1000).toVector
  }


  def printFirstTen[T](numbers: Vector[T]): String =
    numbers.take(10).mkString(", ")

  def generateAndPrint(choice: Int): Option[String] = {
    val config = choice match {
      case 1 => Some(GeneratorConfig(0, 10000, IntType, allowDuplicates = false))
      case 2 => Some(GeneratorConfig(-5000, 5000, IntType, allowDuplicates = false))
      case 3 => Some(GeneratorConfig(-5000, 5000, IntType, allowDuplicates = true))
      case 4 => Some(GeneratorConfig(0, 10000, DoubleType, allowDuplicates = false))
      case 5 => Some(GeneratorConfig(-5000, 5000, DoubleType, allowDuplicates = false))
      case 6 => Some(GeneratorConfig(-5000, 5000, DoubleType, allowDuplicates = true))
      case _ => None
    }

    config.map {
      case c @ GeneratorConfig(_, _, IntType, _) => printFirstTen(generateNums[Int](c))
      case c @ GeneratorConfig(_, _, DoubleType, _) => printFirstTen(generateNums[Double](c))
    }
  }
}

// Usage
RandomNumGenerator.generateAndPrint(4).foreach(println)





































