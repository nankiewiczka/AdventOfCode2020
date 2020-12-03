import scala.io.Source
import scala.util.Using

object Day2 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day2.txt")) {
      source =>
        val lines = source.getLines().toList
        println(getNumberOfValidPasswords(lines, isPasswordValidFirstPart, 0))
        println(getNumberOfValidPasswords(lines, isPasswordValidSecondPart, 1))
    }
  }

  def getNumberOfValidPasswords(lines: List[String], isPasswordValid: (String, Int) => Boolean, indexZero: Int): Int = {
    getNumberOfValidPasswords(lines, isPasswordValid, 0, indexZero)
  }

  def getNumberOfValidPasswords(lines: List[String], isPasswordValid: (String, Int) => Boolean, i: Int, indexZero: Int): Int = {
    if (i < lines.length) {
      val result =
        if (isPasswordValid(lines(i), indexZero)) 1
        else 0
      result + getNumberOfValidPasswords(lines, isPasswordValid, i + 1, indexZero)
    }
    else 0
  }

  def isPasswordValidFirstPart(line: String, indexZero: Int): Boolean = {
    val fields = line.split(" ")
    val occurrences = findAllLetterOccurrences(extractPassword(fields), extractExpectedLetter(fields), 0)

    areAllOccurrencesInRange(extractMinIndex(fields, indexZero), extractMaxIndex(fields, indexZero), occurrences)
  }

  private def areAllOccurrencesInRange(min: Int, max: Int, occurrences: Int): Boolean = {
    if (occurrences >= min && occurrences <= max) true
    else false
  }

  def isPasswordValidSecondPart(line: String, indexZero: Int): Boolean = {
    val fields = line.split(" ")

    isLetterOnlyOnOnePosition(extractPassword(fields), extractExpectedLetter(fields), extractMinIndex(fields,
      indexZero), extractMaxIndex(fields, indexZero))
  }

  def findAllLetterOccurrences(password: String, letter: Char, index: Int): Int = {
    if (index < password.length) {
      val current =
        if (isLetterOnPosition(password, letter, index)) 1
        else 0
      current + findAllLetterOccurrences(password, letter, index + 1)
    }
    else 0
  }

  private def extractMinIndex(fields: Array[String], indexZero: Int) = {
    fields(0).split("-").head.toInt - indexZero
  }

  private def extractMaxIndex(fields: Array[String], indexZero: Int) = {
    fields(0).split("-").last.toInt - indexZero
  }

  private def extractExpectedLetter(fields: Array[String]) = {
    fields(1).replace(":", "").trim.charAt(0)
  }

  private def extractPassword(fields: Array[String]) = {
    fields(2)
  }

  def isLetterOnlyOnOnePosition(password: String, letter: Char, minPosition: Int, maxPosition: Int): Boolean = {
    isLetterOnPosition(password, letter, minPosition) ^ isLetterOnPosition(password, letter, maxPosition)
  }

  def isLetterOnPosition(password: String, letter: Char, position: Int): Boolean = {
    password.charAt(position) == letter
  }
}
