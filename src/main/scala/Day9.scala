import scala.io.Source
import scala.util.Using

object Day9 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day9.txt")) {
      source =>
        val values = source.getLines().toList.map(_.toLong)
        val preambleLength = 25
        println(findFirstNumberWhichIsNotSumOfPreviousNumbers(values, preambleLength, preambleLength + 1) == 258585477)

    }
  }

  def findFirstNumberWhichIsNotSumOfPreviousNumbers(values: List[Long], preambleLength: Int, index: Int): Long = {
    if (!isNumberSumOfPrevious(values, preambleLength, index)) {
      values(index)
    } else {
      findFirstNumberWhichIsNotSumOfPreviousNumbers(values, preambleLength, index + 1)
    }
  }

  def isNumberSumOfPrevious(values: List[Long], preambleLength: Int, index: Int): Boolean = {
    isNumberSumOfPrevious(values, preambleLength, index, index - preambleLength - 1)
  }

  def isNumberSumOfPrevious(values: List[Long], preambleLength: Int, index: Int, startIndex: Int): Boolean = {
    if (startIndex < index) {
      if (isSumOfPreviousNumbers(values, startIndex, index - preambleLength, index)) {
        true
      } else {
        isNumberSumOfPrevious(values, preambleLength, index, startIndex + 1)
      }
    } else {
      false
    }
  }

  def isSumOfPreviousNumbers(values: List[Long], startIndex: Int, insideStartIndex: Int = 0, index: Int): Boolean = {
    if (insideStartIndex < index) {
      if (values(index) == values(startIndex) + values(insideStartIndex)) {
        true
      } else {
        isSumOfPreviousNumbers(values, startIndex, insideStartIndex + 1, index)
      }
    } else {
      false
    }
  }
}
