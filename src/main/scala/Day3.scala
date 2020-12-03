
import scala.io.Source
import scala.util.Using

object Day3 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day3.txt")) {
      source =>
        val values = source.getLines().toList
        println(firstPart(values))
        println(secondPart(values))
    }
  }

  def firstPart(values: List[String]): Int = {
    countTreesForGivenRightAndDownMove(values, 3, 1)
  }

  def secondPart(values: List[String]): Any = {
    countTreesForGivenRightAndDownMove(values, 1, 1) *
      countTreesForGivenRightAndDownMove(values, 3, 1) *
      countTreesForGivenRightAndDownMove(values, 5, 1) *
      countTreesForGivenRightAndDownMove(values, 7, 1) *
      countTreesForGivenRightAndDownMove(values, 1, 2)
  }

  def countTreesForGivenRightAndDownMove(lines: List[String], right: Int, down: Int): Int = {
    countTrees(lines, 0, 0, right, down)
  }

  def countTrees(values: List[String], horizontal: Int, vertical: Int, right: Int, down: Int): Int = {
    val horizontalLength = values(0).length
    val treeSign = '#'
    if (vertical < values.length) {
      val resultForLine =
        if (values(vertical).charAt(horizontal % horizontalLength) == treeSign) 1
        else 0
      resultForLine + countTrees(values, horizontal + right, vertical + down, right, down)
    }
    else 0
  }

}




