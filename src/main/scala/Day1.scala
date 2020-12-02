import scala.io.Source
import scala.util.Using

object Day1 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day1.txt")) {
      source =>
        val values = source.getLines().toList.map(_.toInt)
        println(firstPart(values))
        println(secondPart(values))
    }
  }

  def firstPart(values: List[Int]): Int = {
    var result: Int = 0
    for (i <- values.indices) {
      var j = i
      while (j < values.length) {
        if (values(i) + values(j) == 2020) {
          result = values(i) * values(j)
        }
        j += 1
      }
    }
    result
  }

  def secondPart(values: List[Int]): Int = {
    var result: Int = 0
    for (i <- values.indices) {
      var j = i
      while (j < values.length) {
        if (values(i) + values(j) <= 2020) {
          var k = j
          while (k < values.length) {
            if (values(i) + values(j) + values(k) == 2020) {
              result = values(i) * values(j) * values(k)
            }
            k += 1
          }
        }
        j += 1
      }
    }
    result
  }
}
