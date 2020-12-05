import scala.io.Source
import scala.util.Using

object Day5 {

  def main(args: Array[String]) = {
    val rowSpecLength = 7
    val startRowRegion = 0
    val endRowRegion = 127
    val startColumnRegion = 0
    val endColumnRegion = 7

    Using.resource(Source.fromFile("src/main/data/day5.txt")) {
      source =>
        val values = source.getLines().toList
        val highestSeatId = getHighestSeatId(values, rowSpecLength, startRowRegion,
          endRowRegion, startColumnRegion, endColumnRegion)
        val lowestSeatId = getLowestSeatId(values, rowSpecLength,
          startRowRegion, endRowRegion, startColumnRegion, endColumnRegion)

        println(highestSeatId)
        println(
          getYourSeatId(
            values,
            lowestSeatId,
            highestSeatId,
            rowSpecLength,
            startRowRegion,
            endRowRegion,
            startColumnRegion,
            endColumnRegion))
    }
  }

  def getHighestSeatId(values: List[String],
                       rowSpecLength: Int,
                       startRowRegion: Int,
                       endRowRegion: Int,
                       startColumnRegion: Int,
                       endColumnRegion: Int): Int = {
    getHighestSeatId(
      values,
      0,
      0,
      rowSpecLength,
      startRowRegion,
      endRowRegion,
      startColumnRegion,
      endColumnRegion)
  }

  def getYourSeatId(values: List[String],
                    lowestId: Int,
                    highestId: Int,
                    rowSpecLength: Int,
                    startRowRegion: Int,
                    endRowRegion: Int,
                    startColumnRegion: Int,
                    endColumnRegion: Int): Int = {
    val sum = ((lowestId + highestId) * (highestId - lowestId + 1)) / 2
    val sumOfIds = getSumOfAllSeatsId(
      values,
      0,
      rowSpecLength,
      startRowRegion,
      endRowRegion,
      startColumnRegion,
      endColumnRegion)

    sum - sumOfIds
  }

  def getHighestSeatId(values: List[String],
                       index: Int,
                       maxId: Int,
                       rowSpecLength: Int,
                       startRowRegion: Int,
                       endRowRegion: Int,
                       startColumnRegion: Int,
                       endColumnRegion: Int): Int = {
    if (index < values.length) {
      val currentId = getSeatId(
        getRowNumber(values(index), rowSpecLength, 0, startRowRegion, endRowRegion),
        getColumnNumber(values(index), rowSpecLength, startColumnRegion, endColumnRegion))

      if (currentId > maxId) getHighestSeatId(
        values,
        index + 1,
        currentId,
        rowSpecLength,
        startRowRegion,
        endRowRegion,
        startColumnRegion,
        endColumnRegion)
      else getHighestSeatId(
        values,
        index + 1,
        maxId,
        rowSpecLength,
        startRowRegion,
        endRowRegion,
        startColumnRegion,
        endColumnRegion)
    }
    else maxId
  }

  def getLowestSeatId(values: List[String],
                      rowSpecLength: Int,
                      startRowRegion: Int,
                      endRowRegion: Int,
                      startColumnRegion: Int,
                      endColumnRegion: Int): Int = {
    getLowestSeatId(
      values,
      0,
      Int.MaxValue,
      rowSpecLength,
      startRowRegion,
      endRowRegion,
      startColumnRegion,
      endColumnRegion)
  }

  def getLowestSeatId(values: List[String],
                      index: Int, minId:
                      Int, rowSpecLength: Int,
                      startRowRegion: Int,
                      endRowRegion: Int,
                      startColumnRegion: Int,
                      endColumnRegion: Int): Int = {
    if (index < values.length) {
      val currentId = getSeatId(
        getRowNumber(values(index),
          rowSpecLength,
          0,
          startRowRegion,
          endRowRegion),
        getColumnNumber(
          values(index),
          rowSpecLength,
          startColumnRegion,
          endColumnRegion))

      if (currentId < minId) getLowestSeatId(
        values,
        index + 1,
        currentId,
        rowSpecLength,
        startRowRegion,
        endRowRegion,
        startColumnRegion,
        endColumnRegion)
      else getLowestSeatId(
        values,
        index + 1,
        minId,
        rowSpecLength,
        startRowRegion,
        endRowRegion,
        startColumnRegion,
        endColumnRegion)
    }
    else minId
  }

  def getSumOfAllSeatsId(values: List[String],
                         index: Int,
                         rowSpecLength: Int,
                         startRowRegion: Int,
                         endRowRegion: Int,
                         startColumnRegion: Int, endColumnRegion: Int): Int = {
    if (index < values.length) {
      val id = getSeatId(
        getRowNumber(values(index),
          rowSpecLength,
          0,
          startRowRegion,
          endRowRegion),
        getColumnNumber(values(index), rowSpecLength, startColumnRegion, endColumnRegion))

      id +
        getSumOfAllSeatsId(
          values,
          index + 1,
          rowSpecLength,
          startRowRegion,
          endRowRegion,
          startColumnRegion,
          endColumnRegion)
    }
    else 0
  }

  def getRowNumber(line: String, rowSpecLength: Int, index: Int, startRowRegion: Int, endRowRegion: Int): Int = {
    val lowerHalfSign = 'F'
    val letter = line.charAt(index)
    if (index < rowSpecLength - 1) {
      val distance = (endRowRegion - startRowRegion) / 2
      if (letter == lowerHalfSign) getRowNumber(line, rowSpecLength, index + 1, startRowRegion, startRowRegion + distance)
      else getRowNumber(line, rowSpecLength, index + 1, endRowRegion - distance, endRowRegion)
    }
    else {
      if (letter == lowerHalfSign) startRowRegion
      else endRowRegion
    }
  }

  def getColumnNumber(line: String, index: Int, startColumnRegion: Int, endColumnRegion: Int): Int = {
    val lowerHalfSign = 'L'
    val letter = line.charAt(index)
    if (index < line.length - 1) {
      val distance = (endColumnRegion - startColumnRegion) / 2
      if (letter == lowerHalfSign) getColumnNumber(line, index + 1, startColumnRegion, startColumnRegion + distance)
      else getColumnNumber(line, index + 1, endColumnRegion - distance, endColumnRegion)
    }
    else {
      if (letter == lowerHalfSign) startColumnRegion
      else endColumnRegion
    }
  }

  def getSeatId(rowNumber: Int, columnNumber: Int): Int = {
    rowNumber * 8 + columnNumber
  }
}
