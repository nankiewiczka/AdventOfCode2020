import scala.io.Source
import scala.util.Using

object Day4 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day4.txt")) {
      source =>
        val values = source.getLines().toList
        println(getNumberOfValidPassports(values, 0, List.empty, areAllRequiredFieldsPresent))
        println(getNumberOfValidPassports(values, 0, List.empty, areAllRequiredFieldsValid))
    }
  }

  def getNumberOfValidPassports(values: List[String], index: Int, currentFieldsInPassport: List[String],
                                isPasswordValid: List[String] => Boolean): Int = {
    if (index < values.length) {
      if (values(index) == "") {
        val currentResult =
          if (isPasswordValid(currentFieldsInPassport)) 1
          else 0

        currentResult + getNumberOfValidPassports(values, index + 1, List.empty, isPasswordValid)
      }
      else {
        getNumberOfValidPassports(values, index + 1,
          currentFieldsInPassport.appendedAll(values(index).split(" ")), isPasswordValid)
      }
    }
    else {
      if (isPasswordValid(currentFieldsInPassport)) 1
      else 0
    }
  }

  def areAllRequiredFieldsPresent(current: List[String]): Boolean = {
    areAllRequiredFieldsPresent(current, 0, List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"))
  }

  def areAllRequiredFieldsPresent(current: List[String], index: Int, fieldsPrefixes: List[String]): Boolean = {
    if (index < fieldsPrefixes.length - 1) {
      isFieldPresent(current, fieldsPrefixes(index)) && areAllRequiredFieldsPresent(current, index + 1, fieldsPrefixes)
    }
    else isFieldPresent(current, fieldsPrefixes(index))
  }

  def isFieldPresent(current: List[String], fieldPrefix: String): Boolean = {
    current.exists(x => x.startsWith(fieldPrefix))
  }

  def areAllRequiredFieldsValid(current: List[String]): Boolean = {
    areAllRequiredFieldsPresent(current) && areAllFieldsValid(current)
  }

  def areAllFieldsValid(current: List[String]): Boolean = {
    isBirthYearValid(extractFieldValue(current, "byr").toInt) &&
      isIssueYearValid(extractFieldValue(current, "iyr").toInt) &&
      isExpirationYearValid(extractFieldValue(current, "eyr").toInt) &&
      isHeightValid(extractFieldValue(current, "hgt")) &&
      isHairColourValid(extractFieldValue(current, "hcl")) &&
      isEyeColourValid(extractFieldValue(current, "ecl")) &&
      isPassportIdValid(extractFieldValue(current, "pid"))
  }

  def extractFieldValue(current: List[String], field: String): String = {
    current.find(x => x.startsWith(field)).map(x => x.replaceAll(field + ":", "")).get
  }

  def isBirthYearValid(year: Int): Boolean = {
    year >= 1920 && year <= 2002
  }

  def isIssueYearValid(year: Int): Boolean = {
    year >= 2010 && year <= 2020
  }

  def isExpirationYearValid(year: Int): Boolean = {
    year >= 2020 && year <= 2030
  }

  def isHeightValid(height: String): Boolean = {
    if (height.matches("[0-9]+(cm|in)")) {
      val unit = height.substring(height.length - 2)
      val value = height.substring(0, height.length - 2).toInt
      if (unit == "cm") value >= 150 && value <= 193
      else value >= 59 && value <= 76
    }
    else false
  }

  def isHairColourValid(colour: String): Boolean = {
    colour.matches("#[0-9a-f]{6}")
  }

  def isEyeColourValid(colour: String): Boolean = {
    val validColor = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    colour.length == 3 && validColor.contains(colour)
  }

  def isPassportIdValid(passportId: String): Boolean = {
    passportId.matches("[0-9]{9}")
  }
}
