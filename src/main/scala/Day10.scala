import scala.io.Source
import scala.util.Using

object Day10 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day10.txt")) {
      source =>
        val values = source.getLines().toList.map(_.toInt)
        println(getMultiplicationOfJoltNumbers(values) == 1980)
        println(getDistinctNumberOfTotalWayOfConnection(values, List(0)) == 19208)
    }
  }

  def getMultiplicationOfJoltNumbers(values: List[Int],
                                     adapterRate: Int = 0,
                                     oneJoltAdaptersAmount: Int = 0,
                                     threeJoltAdaptersAmount: Int = 0): Int = {
    if (values.nonEmpty) {
      val adapter = findAdapterWithAnyOfPossibleRate(values, adapterRate)
      val difference = adapter - adapterRate
      val currentOneJolt =
        if (difference == 1) oneJoltAdaptersAmount + 1
        else oneJoltAdaptersAmount
      val currentThreeJolt =
        if (difference == 3) threeJoltAdaptersAmount + 1
        else threeJoltAdaptersAmount
      getMultiplicationOfJoltNumbers(values.filter(_ > adapter), adapter, currentOneJolt, currentThreeJolt)
    }
    else {
      oneJoltAdaptersAmount * (threeJoltAdaptersAmount + 1)
    }
  }

  def getDistinctNumberOfTotalWayOfConnection(values: List[Int], previous: List[Int]): Int = {
    val a = getNumberOfConnections(values, previous, List.empty, 0)
    if (a != previous) {
      getDistinctNumberOfTotalWayOfConnection(values, a)
    } else {
      previous.size
    }
  }

  def getNumberOfConnections(values: List[Int],
                             connections: List[Int],
                             results: List[Int], index: Int = 0): List[Int] = {
    if (index < connections.length) {
      val currentConnections = getAllAdaptersAbleToConnectToAdapter(values, connections(index))
      getNumberOfConnections(values, connections, results ++ currentConnections, index + 1)
    } else {
      results
    }
  }

  def findAdapterWithProvidedRate(values: List[Int], adapterRate: Int, rate: Int): Option[Int] = {
    Some(adapterRate + rate).filter(values.contains)
  }

  def findAdapterWithAnyOfPossibleRate(values: List[Int], adapterRate: Int, rate: Int = 1): Int = {
    values.find(x => x == adapterRate + rate)
      .getOrElse(values.find(x => x == adapterRate + rate + 1)
        .getOrElse(values.find(x => x == adapterRate + rate + 2).get))
  }

  def getAllAdaptersAbleToConnectToAdapter(values: List[Int], adapterRate: Int): List[Int] = {
    val a = findAdapterWithProvidedRate(values, adapterRate, 1)
    val b = findAdapterWithProvidedRate(values, adapterRate, 2)
    val c = findAdapterWithProvidedRate(values, adapterRate, 3)

    val listOfAllAdapters = List(a, b, c).filter(x => x.isDefined).map(x => x.get)

    if (adapterRate == values.max) {
      listOfAllAdapters.appendedAll(List(values.max))
    }
    else {
      listOfAllAdapters
    }
  }

}
