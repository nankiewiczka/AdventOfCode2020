import scala.io.Source
import scala.util.Using

object Day13 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day13.txt")) {
      source =>
        val values = source.getLines().toList
        val departmentTimestamp = values(0).toInt
        val busIds = values(1).split(",").filter(x => x != "x").toList.map(_.toInt)

        println(getNumberOdBusMultipliedByMinutesToWait(departmentTimestamp, busIds) == 333)
        println(getTimestamp(values(1).split(",").toList))

    }
  }

  def getNumberOdBusMultipliedByMinutesToWait(departmentTimestamp: Int,
                                              busIds: List[Int],
                                              index: Integer = 0,
                                              earliestBusId: Int = 0,
                                              earliestTimestamp: Int = Int.MaxValue): Int = {
    if (index < busIds.length) {
      val busId = busIds(index)
      val departureTimeForBus = getDepartureTimeForBus(departmentTimestamp, busId)
      val calculatedBusId =
        if (departureTimeForBus < earliestTimestamp) busId
        else earliestBusId
      val calculatedEarliestTimestamp =
        if (departureTimeForBus < earliestTimestamp) departureTimeForBus
        else earliestTimestamp
      getNumberOdBusMultipliedByMinutesToWait(departmentTimestamp, busIds, index + 1, calculatedBusId,
        calculatedEarliestTimestamp)
    } else {
      earliestBusId * (earliestTimestamp - departmentTimestamp)
    }
  }

  private def getDepartureTimeForBus(departmentTimestamp: Int, busId: Int): Int = {
    if (departmentTimestamp % busId == 0) {
      departmentTimestamp
    }
    else {
      val factor = departmentTimestamp / busId
      busId * (factor + 1)
    }
  }

  def isTimestampOkForBusIdWithOffset(timestamp: Long, busId: Int, offset: Int): Boolean = {
    if (busId == "x") true
    else {
      (timestamp + offset) % busId == 0
    }
  }

  def isCorrectTimestamp(timestamp: Long, busIds: List[String], index: Int = 0): Boolean = {
    if (index < busIds.length) {
      if (isTimestampOkForBusIdWithOffset(timestamp, busIds(index).toInt, index)) {
        isCorrectTimestamp(timestamp, busIds, index + 1)
      }
      else {
        false
      }
    } else {
      true
    }
  }

  def getTimestamp(busIds: List[String]): Long = {
    var timestamp = 100000000000000L
    while (!isCorrectTimestamp(timestamp, busIds)) {
      timestamp = timestamp + 1
    }
    timestamp
  }
}
