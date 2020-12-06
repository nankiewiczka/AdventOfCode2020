import scala.io.Source
import scala.util.Using

object Day6 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day6.txt")) {
      source =>
        val values = source.getLines().toList
        println(getSumOfAllPositiveQuestionsInGroup(values))
        println(getSumOfSamePositiveQuestionsInGroup(values, values(0).toSet))
    }
  }

  def getSumOfAllPositiveQuestionsInGroup(values: List[String],
                                          positiveAnswersInCurrentGroup: Set[Char] = Set.empty,
                                          index: Int = 0): Int = {
    if (index < values.length) {
      if (values(index) == "") {
        positiveAnswersInCurrentGroup.size + getSumOfAllPositiveQuestionsInGroup(values, Set.empty, index + 1)
      }
      else {
        val positiveAnswers = values(index).toSet
        getSumOfAllPositiveQuestionsInGroup(values, positiveAnswersInCurrentGroup ++ positiveAnswers, index + 1)
      }
    } else {
      positiveAnswersInCurrentGroup.size
    }
  }

  def getSumOfSamePositiveQuestionsInGroup(values: List[String],
                                           commonPositiveAnswers: Set[Char],
                                           index: Int = 1): Int = {
    if (index < values.length) {
      if (values(index) == "") {
        commonPositiveAnswers.size + getSumOfSamePositiveQuestionsInGroup(values, values(index + 1).toSet, index + 2)
      }
      else {
        val positiveAnswers = values(index).toSet
        getSumOfSamePositiveQuestionsInGroup(values, commonPositiveAnswers.intersect(positiveAnswers), index + 1)
      }
    } else {
      commonPositiveAnswers.size
    }
  }
}
