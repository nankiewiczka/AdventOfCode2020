import scala.io.Source
import scala.util.Using

object Day12 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day12.txt")) {
      source =>
        val values = source.getLines().toList
        println(getManhattanDistanceWhenMoveShip(values))
        println(getManhattanDistanceWhenMoveRelativeToShip(values))
    }
  }

  def getManhattanDistanceWhenMoveShip(values: List[String],
                                       verticalDirection: String = "N",
                                       verticalValue: Int = 0,
                                       horizontalDirection: String = "E",
                                       horizontalValue: Int = 0,
                                       face: String = "E",
                                       index: Int = 0): Int = {
    if (index < values.length) {
      val (direction: String, value: Int) = extractDirectionAndValueFromLine(values, index)
      if (direction == "F") {
        if (isVerticalDirection(face)) {
          if (verticalDirection == face) {
            getManhattanDistanceWhenMoveShip(
              values,
              verticalDirection,
              verticalValue + value,
              horizontalDirection,
              horizontalValue,
              face,
              index + 1)
          } else {
            val dir =
              if (value >= verticalValue) {
                if (verticalDirection == "N") "S"
                else "N"
              } else {
                verticalDirection
              }

            getManhattanDistanceWhenMoveShip(
              values,
              dir,
              (verticalValue - value).abs,
              horizontalDirection,
              horizontalValue,
              face,
              index + 1)
          }
        } else {
          if (horizontalDirection == face) {
            getManhattanDistanceWhenMoveShip(
              values,
              verticalDirection,
              verticalValue,
              horizontalDirection,
              horizontalValue + value,
              face,
              index + 1)
          } else {
            val dir =
              if (value >= horizontalValue) {
                if (horizontalDirection == "W") "E"
                else "W"
              } else {
                horizontalDirection
              }

            getManhattanDistanceWhenMoveShip(
              values,
              verticalDirection,
              verticalValue,
              dir,
              (horizontalValue - value).abs,
              face,
              index + 1)
          }
        }
      } else if (isVerticalDirection(direction)) {
        if (direction == verticalDirection) {
          getManhattanDistanceWhenMoveShip(
            values,
            direction,
            verticalValue + value,
            horizontalDirection,
            horizontalValue,
            face,
            index + 1)
        } else {
          val dir =
            if (value >= verticalValue) direction
            else verticalDirection

          getManhattanDistanceWhenMoveShip(
            values,
            dir,
            (verticalValue - value).abs,
            horizontalDirection,
            horizontalValue,
            face,
            index + 1)
        }

      } else if (isHorizontalDirection(direction)) {
        if (direction == horizontalDirection) {
          getManhattanDistanceWhenMoveShip(
            values,
            verticalDirection,
            verticalValue,
            direction,
            horizontalValue + value,
            face,
            index + 1)
        } else {
          val dir =
            if (value >= horizontalValue) direction
            else horizontalDirection

          getManhattanDistanceWhenMoveShip(
            values,
            verticalDirection,
            verticalValue,
            dir,
            (horizontalValue - value).abs,
            face,
            index + 1)
        }

      } else {
        val sign = getDirectionSign(direction)
        val changedFaceDirection = getChangedFaceDirection(face, value, sign)

        getManhattanDistanceWhenMoveShip(
          values,
          verticalDirection,
          verticalValue,
          horizontalDirection,
          horizontalValue,
          changedFaceDirection,
          index + 1)
      }
    } else {
      verticalValue + horizontalValue
    }
  }

  def getManhattanDistanceWhenMoveRelativeToShip(values: List[String],
                                                 wayPointVerticalDirection: String = "N",
                                                 wayPointVerticalValue: Int = 1,
                                                 wayPointHorizontalDirection: String = "E",
                                                 wayPointHorizontalValue: Int = 10,
                                                 shipVerticalDirection: String = "N",
                                                 shipVerticalValue: Int = 0,
                                                 shipHorizontalDirection: String = "E",
                                                 shipHorizontalValue: Int = 0,
                                                 index: Int = 0): Int = {
    if (index < values.length) {
      val (direction: String, value: Int) = extractDirectionAndValueFromLine(values, index)
      if (direction == "F") {
        val changedShipVerticalDirection =
          if (shipVerticalDirection != wayPointVerticalDirection &&
            wayPointVerticalValue * value >= shipVerticalValue) {
            wayPointVerticalDirection
          }
          else {
            shipVerticalDirection
          }
        val changedWayShipVerticalValue =
          if (shipVerticalDirection != wayPointVerticalDirection) {
            (value * wayPointVerticalValue - shipVerticalValue).abs
          }
          else {
            value * wayPointVerticalValue + shipVerticalValue
          }

        val changedShipHorizontalDirection =
          if (shipHorizontalDirection != wayPointHorizontalDirection &&
            wayPointHorizontalValue * value >= shipHorizontalValue) {
            wayPointHorizontalDirection
          }
          else {
            shipHorizontalDirection
          }
        val changedShipHorizontalValue =
          if (shipHorizontalDirection != wayPointHorizontalDirection) {
            (value * wayPointHorizontalValue -
              shipHorizontalValue).abs
          }
          else {
            value * wayPointHorizontalValue + shipHorizontalValue
          }

        getManhattanDistanceWhenMoveRelativeToShip(
          values,
          wayPointVerticalDirection,
          wayPointVerticalValue,
          wayPointHorizontalDirection,
          wayPointHorizontalValue,
          changedShipVerticalDirection,
          changedWayShipVerticalValue,
          changedShipHorizontalDirection,
          changedShipHorizontalValue,
          index + 1)

      } else if (isVerticalDirection(direction)) {
        if (direction == wayPointVerticalDirection) {
          getManhattanDistanceWhenMoveRelativeToShip(
            values,
            direction,
            wayPointVerticalValue + value,
            wayPointHorizontalDirection,
            wayPointHorizontalValue,
            shipVerticalDirection,
            shipVerticalValue,
            shipHorizontalDirection,
            shipHorizontalValue,
            index + 1)
        } else {
          val dir =
            if (value >= wayPointVerticalValue) direction
            else wayPointVerticalDirection

          getManhattanDistanceWhenMoveRelativeToShip(
            values,
            dir,
            (wayPointVerticalValue - value).abs,
            wayPointHorizontalDirection,
            wayPointHorizontalValue,
            shipVerticalDirection,
            shipVerticalValue,
            shipHorizontalDirection,
            shipHorizontalValue,
            index + 1)
        }

      } else if (isHorizontalDirection(direction)) {
        if (direction == wayPointHorizontalDirection) {
          getManhattanDistanceWhenMoveRelativeToShip(
            values,
            wayPointVerticalDirection,
            wayPointVerticalValue,
            direction,
            wayPointHorizontalValue + value,
            shipVerticalDirection,
            shipVerticalValue,
            shipHorizontalDirection,
            shipHorizontalValue,
            index + 1)
        } else {
          val dir =
            if (value >= wayPointHorizontalValue) direction
            else wayPointHorizontalDirection

          getManhattanDistanceWhenMoveRelativeToShip(
            values,
            wayPointVerticalDirection,
            wayPointVerticalValue,
            dir,
            (wayPointHorizontalValue - value).abs,
            shipVerticalDirection,
            shipVerticalValue,
            shipHorizontalDirection,
            shipHorizontalValue,
            index + 1)
        }

      } else {
        val sign = getDirectionSign(direction)

        val changedFirst = (getChangedFaceDirection(wayPointVerticalDirection, value, sign), wayPointVerticalValue)
        val changedSecond = (getChangedFaceDirection(wayPointHorizontalDirection, value, sign), wayPointHorizontalValue)

        val changedWayPointVerticalDirection =
          if (isVerticalDirection(changedFirst._1)) changedFirst._1
          else changedSecond._1
        val changedWayPointVerticalValue =
          if (isVerticalDirection(changedFirst._1)) changedFirst._2
          else changedSecond._2.toInt

        val changedWayPointHorizontalDirection =
          if (isHorizontalDirection(changedSecond._1)) changedSecond._1
          else changedFirst._1
        val changedWayPointHorizontalValue =
          if (isHorizontalDirection(changedSecond._1)) changedSecond._2.toInt
          else changedFirst._2

        getManhattanDistanceWhenMoveRelativeToShip(
          values,
          changedWayPointVerticalDirection,
          changedWayPointVerticalValue,
          changedWayPointHorizontalDirection,
          changedWayPointHorizontalValue,
          shipVerticalDirection,
          shipVerticalValue,
          shipHorizontalDirection,
          shipHorizontalValue,
          index + 1)
      }
    } else {
      shipHorizontalValue + shipVerticalValue
    }
  }

  private def extractDirectionAndValueFromLine(values: List[String], index: Int) = {
    val pattern = "(\\w)(\\d+)".r
    val pattern(direction, value) = values(index)
    (direction, value.toInt)
  }

  private def getDirectionSign(direction: String) = {
    if (direction == "R") 1
    else -1
  }

  def getChangedFaceDirection(previousFaceDirection: String, value: Int, sign: Int): String = {
    val degree = getDegreeFromFace(previousFaceDirection)
    val calculatedDegree = 360 + degree + (sign * value)
    val changedFaceDirection = getFaceDirectionFromDegree(calculatedDegree)
    changedFaceDirection
  }

  def getDegreeFromFace(faceDirection: String): Int = {
    if (faceDirection == "N") 0
    else if (faceDirection == "E") 90
    else if (faceDirection == "S") 180
    else 270
  }

  def getFaceDirectionFromDegree(degree: Int): String = {
    if (degree % 360 == 0) "N"
    else if (degree % 360 == 90) "E"
    else if (degree % 360 == 180) "S"
    else "W"
  }

  private def isVerticalDirection(direction: String): Boolean = {
    direction == "N" || direction == "S"
  }

  private def isHorizontalDirection(direction: String): Boolean = {
    direction == "E" || direction == "W"
  }
}
