import scala.io.Source
import scala.util.Using

object Day11 {
  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day11.txt")) {
      source =>
        val values = source.getLines().toList
        println(
          getNumberOfOccupiedSeatsAfterNoSeatChangeState(isAdjacentSeatOccupied, 4, values))
        println(
          getNumberOfOccupiedSeatsAfterNoSeatChangeState(isVisibleSeatOccupied, 5, values))
    }
  }

  def getNumberOfOccupiedSeatsAfterNoSeatChangeState(visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int)
    => Boolean,
                                                     numberOfAcceptedOccupiedSeats: Int,
                                                     previousLayout: List[String]): Int = {
    val layout = doRoundUntilAnyChanges(visibilityRuleIfSeatOccupied, numberOfAcceptedOccupiedSeats, previousLayout)
    getNumberOfOccupiedSeats(layout)
  }

  def doRoundUntilAnyChanges(visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int) => Boolean,
                             numberOfAcceptedOccupiedSeats: Int,
                             previousLayout: List[String]): List[String] = {
    val seatLayout = getSeatLayoutAfterRound(
      visibilityRuleIfSeatOccupied,
      numberOfAcceptedOccupiedSeats,
      previousLayout,
      previousLayout)

    if (seatLayout == previousLayout) {
      seatLayout
    } else {
      doRoundUntilAnyChanges(visibilityRuleIfSeatOccupied, numberOfAcceptedOccupiedSeats, seatLayout)
    }
  }

  def getSeatLayoutAfterRound(visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int) => Boolean,
                              numberOfAcceptedOccupiedSeats: Int,
                              previousLayout: List[String],
                              currentLayout: List[String],
                              index: Int = 0): List[String] = {
    if (index < previousLayout.length) {
      val layout = doRoundOnRow(
        visibilityRuleIfSeatOccupied,
        numberOfAcceptedOccupiedSeats,
        previousLayout,
        currentLayout,
        index)

      getSeatLayoutAfterRound(
        visibilityRuleIfSeatOccupied,
        numberOfAcceptedOccupiedSeats,
        previousLayout,
        layout,
        index + 1)
    } else {
      currentLayout
    }
  }

  def doRoundOnRow(visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int) => Boolean,
                   numberOfAcceptedOccupiedSeats: Int,
                   previousLayout: List[String],
                   currentLayout: List[String],
                   row: Int,
                   column: Int = 0): List[String] = {
    val occupiedSeatSign = '#'
    val emptySeatSign = 'L'
    if (column < previousLayout(row).length) {
      val previousRow = previousLayout(row)
      val currentRow = currentLayout(row)
      val layoutAfterRoundInRow =
        if (isSeatEmptyAndNoneSeatsAreOccupiedAccoridingToRule(
          visibilityRuleIfSeatOccupied,
          previousLayout,
          previousRow,
          row,
          column,
          emptySeatSign)) {
          currentLayout.updated(row, changeSeatStatusInRow(column, currentRow, occupiedSeatSign))
        } else if (isSeatOccupiedAndFourOrMoreSeatsAreOccupiedAccordingToRule(
          numberOfAcceptedOccupiedSeats,
          visibilityRuleIfSeatOccupied,
          previousLayout,
          previousRow,
          row,
          column,
          occupiedSeatSign)) {
          currentLayout.updated(row, changeSeatStatusInRow(column, currentRow, emptySeatSign))
        } else {
          currentLayout
        }
      doRoundOnRow(
        visibilityRuleIfSeatOccupied,
        numberOfAcceptedOccupiedSeats,
        previousLayout,
        layoutAfterRoundInRow,
        row,
        column + 1)
    } else {
      currentLayout
    }
  }

  def getNumberOccupiedSeatsInRow(seatsRow: String, index: Int = 0): Int = {
    if (index < seatsRow.length) {
      val result =
        if (seatsRow.charAt(index) == '#') {
          1
        }
        else {
          0
        }
      result + getNumberOccupiedSeatsInRow(seatsRow, index + 1)
    } else {
      0
    }
  }

  def getNumberOfOccupiedSeats(layout: List[String], index: Int = 0): Int = {
    if (index < layout.length) {
      getNumberOccupiedSeatsInRow(layout(index)) + getNumberOfOccupiedSeats(layout, index + 1)
    } else {
      0
    }
  }

  private def isSeatEmptyAndNoneSeatsAreOccupiedAccoridingToRule(visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int) => Boolean,
                                                                 previousLayout: List[String],
                                                                 previousRow: String,
                                                                 row: Int,
                                                                 column: Int,
                                                                 emptySeatSign: Char) = {
    isSeatAsGivenSign(previousRow, column, emptySeatSign) &&
      getNumberOfAllOccupiedSeats(visibilityRuleIfSeatOccupied, previousLayout, row, column) == 0
  }

  private def isSeatOccupiedAndFourOrMoreSeatsAreOccupiedAccordingToRule(numberOfAcceptedOccupiedSeats: Int,
                                                                         visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int) => Boolean,
                                                                         previousLayout: List[String],
                                                                         previousRow: String,
                                                                         row: Int,
                                                                         column: Int,
                                                                         occupiedSeatSign: Char) = {
    isSeatAsGivenSign(previousRow, column, occupiedSeatSign) &&
      getNumberOfAllOccupiedSeats(visibilityRuleIfSeatOccupied, previousLayout, row, column) >= numberOfAcceptedOccupiedSeats
  }

  private def isSeatAsGivenSign(row: String, column: Int, seatSign: Char): Boolean = {
    row.charAt(column) == seatSign
  }

  private def changeSeatStatusInRow(column: Int, currentRow: String, status: Char): String = {
    currentRow.substring(0, column) + status + currentRow.substring(column + 1)
  }

  def getNumberOfAllOccupiedSeats(visibilityRuleIfSeatOccupied: (List[String], Int, Int, Int, Int) => Boolean,
                                  previousLayout: List[String],
                                  row: Int,
                                  column: Int): Int = {
    getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, -1, 0)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, 1, 0)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, -1, -1)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, -1, 1)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, 0, -1)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, 0, 1)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, 1, -1)) +
      getNumberOfOccupied(visibilityRuleIfSeatOccupied(previousLayout, row, column, 1, 1))
  }

  def isAdjacentSeatOccupied(previousLayout: List[String],
                             row: Int,
                             column: Int,
                             rowOffset: Int,
                             columnOffset: Int): Boolean = {
    val rowsNumber = previousLayout.length
    val columnNumber = previousLayout(0).length
    val rowIndex = row + rowOffset
    val columnIndex = column + columnOffset

    rowIndex >= 0 &&
      rowIndex < rowsNumber &&
      columnIndex >= 0 &&
      columnIndex < columnNumber &&
      previousLayout(rowIndex).charAt(columnIndex) == '#'
  }

  def isVisibleSeatOccupied(previousLayout: List[String],
                            row: Int,
                            column: Int,
                            rowOffset: Int,
                            columnOffset: Int): Boolean = {
    val rowsNumber = previousLayout.length
    val columnNumber = previousLayout(0).length
    val rowIndex = row + rowOffset
    val columnIndex = column + columnOffset

    if (
      rowIndex >= 0 &&
        rowIndex < rowsNumber &&
        columnIndex >= 0 &&
        columnIndex < columnNumber) {
      if (previousLayout(rowIndex).charAt(columnIndex) == '.') {
        isVisibleSeatOccupied(previousLayout, rowIndex, columnIndex, rowOffset, columnOffset)
      } else {
        previousLayout(rowIndex).charAt(columnIndex) == '#'
      }
    } else {
      false
    }
  }

  def getNumberOfOccupied(isOccupied: Boolean): Int = {
    if (isOccupied) 1
    else 0
  }
}
