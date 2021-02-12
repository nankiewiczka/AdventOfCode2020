import scala.io.Source
import scala.util.Using

object Day14 {


  def getSumOfAllValuesInMemory(values: List[String],
                                index: Int = 0,
                                mask: String = "",
                                valuesInMemory: Map[Int, BigInt] = Map.empty[Int, BigInt]): BigInt = {
    val MaskLength = 36

    def isMaskDefinition = {
      values(index).startsWith("mask")
    }

    def getMask: String = {
      val pattern = "mask = ([X10]+)".r
      val pattern(mask) = values(index)
      mask
    }

    def getMemoryIndexAndValue = {
      val pattern = "mem\\[([0-9]+)\\] = ([0-9]+)".r
      val pattern(memIndex, value) = values(index)
      (memIndex, value)
    }

    def getValueWithFixedLength(value: Int): String = {
      val binaryValue = value.toBinaryString
      if (binaryValue.length < MaskLength) {
        "0" * (MaskLength - binaryValue.length) + binaryValue
      } else {
        binaryValue
      }
    }

    def getValueToWriteToMemory(valueToWrite: String, index: Int = 0): BigInt = {
      if (index < valueToWrite.length) {
        if (mask(index) == 'X') {
          getValueToWriteToMemory(valueToWrite, index + 1)
        } else {
          getValueToWriteToMemory(
            valueToWrite.substring(0, index) + mask(index) + valueToWrite.substring(index + 1), index + 1)
        }
      } else {
        BigInt(valueToWrite, 2)
      }
    }

    if (index < values.length) {
      if (isMaskDefinition) {
        getSumOfAllValuesInMemory(values, index + 1, getMask, valuesInMemory)
      } else {
        val (memIndex: String, value: String) = getMemoryIndexAndValue
        val valueToMemory = getValueToWriteToMemory(getValueWithFixedLength(value.toInt))
        getSumOfAllValuesInMemory(values, index + 1, mask, valuesInMemory + (memIndex.toInt -> valueToMemory))
      }
    } else {
      valuesInMemory.values.sum
    }
  }

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day14.txt")) {
      source =>
        val values = source.getLines().toList
        println(getSumOfAllValuesInMemory(values))
    }
  }
}
