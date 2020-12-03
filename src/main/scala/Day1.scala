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
    get2NumbersProduct(values, 0, 0)
  }

  def get2NumbersProduct(values: List[Int], result: Int, i: Int): Int = {
    if (result == 0 && i < values.length) {
      get2NumbersProduct(values, get2NumbersProduct(values, result, i, i + 1), i + 1)
    }
    else result
  }

  def get2NumbersProduct(values: List[Int], result: Int, i: Int, j: Int): Int = {
    if (result == 0 && j < values.length) {
      val currentResult =
        if (values(i) + values(j) == 2020) values(i) * values(j)
        else 0
      get2NumbersProduct(values, currentResult, i, j + 1)
    }
    else result
  }

  def secondPart(values: List[Int]): Int = {
    get3NumbersProduct(values, 0, 0)
  }

  def get3NumbersProduct(values: List[Int], result: Int, i: Int): Int = {
    if (result == 0 && i < values.length) {
      get3NumbersProduct(values, get3NumbersProduct(values, result, i, i + 1), i + 1)
    }
    else result
  }

  def get3NumbersProduct(values: List[Int], result: Int, i: Int, j: Int): Int = {
    if (result == 0 && j < values.length) {
      get3NumbersProduct(values, get3NumbersProduct(values, result, i, j, j + 1), i, j + 1)
    }
    else result
  }

  def get3NumbersProduct(values: List[Int], result: Int, i: Int, j: Int, k: Int): Int = {
    if (result == 0 && k < values.length) {
      val currentResult =
        if (values(i) + values(j) + values(k) == 2020) values(i) * values(j) * values(k)
        else 0
      get3NumbersProduct(values, currentResult, i, j, k + 1)
    }
    else result
  }
}
