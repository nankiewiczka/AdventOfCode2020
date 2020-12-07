import scala.io.Source
import scala.util.Using

object Day7 {


  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day7.txt")) {
      source =>
        val values = source.getLines().toList
        val dataMap = parseLines(Map.empty, values, 0, getDataForFirstPart)
        val dataMap2 = parseLines(Map.empty, values, 0, getDataForSecondPart)
        println(getNumberOfColorsThatContainCertainBag(dataMap, "shiny gold"))
        println(getNumberOfBagsInCertainBag(dataMap2, "shiny gold"))
    }
  }

  def parseLines(dataMap: Map[String, Map[String, Int]],
                 values: List[String],
                 index: Int = 0,
                 parse: (Map[String, Map[String, Int]], List[String], String, Int)
                   => Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
    if (index < values.length) {
      parseLines(parseLine(dataMap, values(index), parse), values, index + 1, parse)
    } else {
      dataMap
    }
  }

  def parseLine(dataMap: Map[String, Map[String, Int]],
                line: String,
                parse: (Map[String, Map[String, Int]], List[String], String, Int)
                  => Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
    if (line.contains("no other bags")) {
      dataMap
    } else {
      val values = line.split("bags contain")
      val mapColor = values(0).trim
      val colorsWithNumber = values(1).split(",").toList
      parse(dataMap, colorsWithNumber, mapColor, 0)
    }
  }

  def getDataForFirstPart(dataMap: Map[String, Map[String, Int]],
                          bagsWithNumbers: List[String],
                          innerColor: String,
                          index: Int = 0): Map[String, Map[String, Int]] = {
    if (index < bagsWithNumbers.length) {
      val (amount, outerColor) = parseBagColorAndAmount(bagsWithNumbers, index)

      if (dataMap.contains(outerColor)) {
        val currentInsideMap = dataMap(outerColor)
        if (currentInsideMap.contains(innerColor)) {
          val currentInsideMap = dataMap(outerColor) + (innerColor -> (dataMap(outerColor)(innerColor) + amount.toInt))
          val currentOuterMap = dataMap + (outerColor -> currentInsideMap)
          getDataForFirstPart(currentOuterMap, bagsWithNumbers, innerColor, index + 1)
        } else {
          val currentInsideMap = dataMap(outerColor) + (innerColor -> amount.toInt)
          val currentOuterMap = dataMap + (outerColor -> currentInsideMap)
          getDataForFirstPart(currentOuterMap, bagsWithNumbers, innerColor, index + 1)
        }
      } else {
        val currentOuterMap = dataMap + (outerColor -> Map(innerColor -> amount.toInt))
        getDataForFirstPart(currentOuterMap, bagsWithNumbers, innerColor, index + 1)
      }
    } else {
      dataMap
    }

  }

  def getDataForSecondPart(dataMap: Map[String, Map[String, Int]],
                           bagsWithNumbers: List[String],
                           outerColor: String,
                           index: Int = 0): Map[String, Map[String, Int]] = {
    if (index < bagsWithNumbers.length) {
      val (amount, innerColor) = parseBagColorAndAmount(bagsWithNumbers, index)
      if (dataMap.contains(outerColor)) {
        val currentInsideMap = dataMap(outerColor)
        if (currentInsideMap.contains(innerColor)) {
          val currentInsideMap = dataMap(outerColor) + (innerColor -> (dataMap(outerColor)(innerColor) + amount.toInt))
          val currentOuterMap = dataMap + (outerColor -> currentInsideMap)
          getDataForSecondPart(currentOuterMap, bagsWithNumbers, outerColor, index + 1)
        } else {
          val currentInsideMap = dataMap(outerColor) + (innerColor -> amount.toInt)
          val currentOuterMap = dataMap + (outerColor -> currentInsideMap)
          getDataForSecondPart(currentOuterMap, bagsWithNumbers, outerColor, index + 1)
        }
      } else {
        val currentOuterMap = dataMap + (outerColor -> Map(innerColor -> amount.toInt))
        getDataForSecondPart(currentOuterMap, bagsWithNumbers, outerColor, index + 1)

      }
    } else {
      dataMap
    }
  }

  private def parseBagColorAndAmount(bagsWithNumbers: List[String], index: Int) = {
    val bagWithAmount = bagsWithNumbers(index)
      .replace("bags", "")
      .replace("bag", "")
      .replace(".", "").trim
    val pattern = "([0-9]+) ([A-Za-z]+\\s[A-Za-z]+)".r
    val pattern(amount, color) = bagWithAmount
    (amount, color)
  }

  def getNumberOfColorsThatContainCertainBag(dataMap: Map[String, Map[String, Int]], bagColor: String): Int = {
    getNumberOfColorsThatCanContainBag(dataMap, bagColor).toSet.size
  }

  def getNumberOfColorsThatCanContainBag(dataMap: Map[String, Map[String, Int]], bagColor: String): List[String] = {
    if (dataMap.contains(bagColor)) {
      getNumberOfBagColors(dataMap, dataMap(bagColor).keys.toList)
    } else {
      List.empty[String]
    }
  }

  def getNumberOfBagColors(dataMap: Map[String, Map[String, Int]],
                           bagColorsInsideBag: List[String],
                           index: Int = 0): List[String] = {
    if (index < bagColorsInsideBag.length) {
      List(bagColorsInsideBag(index)) ++
        getNumberOfBagColors(dataMap, bagColorsInsideBag, index + 1) ++
        getNumberOfColorsThatCanContainBag(dataMap, bagColorsInsideBag(index))
    } else {
      List.empty[String]
    }
  }

  def getNumberOfBagsInCertainBag(dataMap: Map[String, Map[String, Int]], bagColor: String): Int = {
    getNumberOfBagsInCertainBag(dataMap, dataMap(bagColor))
  }

  def getNumberOfBagsInCertainBag(dataMap: Map[String, Map[String, Int]],
                                  bagColorsInsideBag: Map[String, Int]): Int = {
    if (bagColorsInsideBag.nonEmpty) {
      val color = bagColorsInsideBag.head._1.toString
      val value = bagColorsInsideBag.head._2.toInt
      value +
        getNumberOfBagsInCertainBag(dataMap, bagColorsInsideBag.removed(color)) +
        (value * getNumberOfBags(dataMap, color))

    } else {
      0
    }
  }

  def getNumberOfBags(dataMap: Map[String, Map[String, Int]], bagColor: String): Int = {
    if (dataMap.contains(bagColor)) {
      getNumberOfBagsInCertainBag(dataMap, dataMap(bagColor))
    } else {
      0
    }
  }
}
