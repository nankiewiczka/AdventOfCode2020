import scala.io.Source
import scala.util.Using

object Day2 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day2.txt")) {
      source =>
        val lines = source.getLines().toList
        println(firstPart(lines))
        println(secondPart(lines))
    }
  }

  def firstPart(lines: List[String]) = {
    var validPasswords = 0
    lines.foreach {
      line =>
        if (isPasswordValidFirstPart(line)) {
          validPasswords += 1
        }
    }
    validPasswords
  }

  def secondPart(lines: List[String]) = {
    var validPasswords = 0
    lines.foreach {
      line =>
        if (isPasswordValidSecondPart(line)) {
          validPasswords += 1
        }
    }
    validPasswords
  }

  def isPasswordValidFirstPart(line: String): Boolean = {
    val fields = line.split(" ")
    val min = fields(0).split("-").head.toInt
    val max = fields(0).split("-").last.toInt
    val letter = fields(1).replace(":", "").trim.charAt(0)
    val password = fields(2)
    var occurrences = 0
    for (c <- password) {
      if (c == letter) {
        occurrences += 1
      }
    }
    if (occurrences >= min && occurrences <= max) {
      return true
    }
    false
  }

  def isPasswordValidSecondPart(line: String): Boolean = {
    val fields = line.split(" ")
    val minPosition = fields(0).split("-").head.toInt - 1
    val maxPosition = fields(0).split("-").last.toInt - 1
    val letter = fields(1).replace(":", "").trim.charAt(0)
    val password = fields(2)
    var occurrences = 0
    if (password.charAt(minPosition) == letter) {
      occurrences += 1
    }
    if (password.charAt(maxPosition) == letter) {
      occurrences += 1
    }
    if (occurrences == 1) {
      return true
    }
    false
  }
}
