import scala.io.Source
import scala.util.Using

object Day8 {

  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day8.txt")) {
      source =>
        val values = source.getLines().map(x => getOperationInfo(x)).toList
        val NoOperation = "nop"
        val JumpOperation = "jmp"
        val AccumulationOperation = "acc"
        println(getValueOfAccumulator(values, AccumulationOperation, JumpOperation))
        println(getValueOfAccumulatorWhenProgramTerminates(values, AccumulationOperation, JumpOperation, NoOperation))
    }
  }

  def getValueOfAccumulator(values: List[(Boolean, String, Int)],
                            accumulationOperation: String,
                            jumpOperation: String,
                            accumulator: Int = 0,
                            index: Int = 0): Int = {
    if (index < values.length && !values(index)._1) {
      val operation = values(index)._2
      val argument = values(index)._3
      val newOperationInfo = (true, operation, argument)
      if (operation == accumulationOperation) {
        getValueOfAccumulator(
          values.updated(index, newOperationInfo), accumulationOperation, jumpOperation, accumulator + argument, index + 1)
      } else if (operation == jumpOperation) {
        getValueOfAccumulator(
          values.updated(index, newOperationInfo), accumulationOperation, jumpOperation, accumulator, index + argument)
      } else {
        getValueOfAccumulator(
          values.updated(index, newOperationInfo), accumulationOperation, jumpOperation, accumulator, index + 1)
      }
    } else {
      accumulator
    }
  }

  def getValueOfAccumulatorWhenProgramTerminates(values: List[(Boolean, String, Int)],
                                                 accumulationOperation: String,
                                                 jumpOperation: String,
                                                 noOperation: String,
                                                 index: Int = 0): Any = {
    val (status, operation, argument) = changeOneOperation(values, jumpOperation, noOperation, index)

    if (isInfiniteLoopInProgram(values.updated(index, (status, operation, argument)), jumpOperation, accumulationOperation)) {
      getValueOfAccumulatorWhenProgramTerminates(values, accumulationOperation, jumpOperation, noOperation, index + 1)
    } else {
      getValueOfAccumulator(values.updated(index, (status, operation, argument)), accumulationOperation, jumpOperation)
    }
  }

  def isInfiniteLoopInProgram(values: List[(Boolean, String, Int)],
                              jumpOperation: String,
                              accumulationOperation: String,
                              index: Int = 0): Boolean = {
    if (index < values.length && !values(index)._1) {
      val operation = values(index)._2
      val argument = values(index)._3
      val newOperationInfo = (true, operation, argument)
      if (operation == accumulationOperation) {
        isInfiniteLoopInProgram(values.updated(index, newOperationInfo), jumpOperation, accumulationOperation, index + 1)
      } else if (operation == jumpOperation) {
        isInfiniteLoopInProgram(values.updated(index, newOperationInfo), jumpOperation, accumulationOperation, index + argument)
      } else {
        isInfiniteLoopInProgram(values.updated(index, newOperationInfo), jumpOperation, accumulationOperation, index + 1)
      }
    } else if (index >= values.length) {
      false
    } else {
      true
    }
  }

  def getOperationInfo(line: String): (Boolean, String, Int) = {
    val pattern = "(\\w+) (\\+|-)(\\d+)".r
    val pattern(operation, sign, argument) = line
    val factor =
      if (sign == "+") 1
      else -1
    (false, operation, factor * argument.toInt)
  }

  private def changeOneOperation(values: List[(Boolean, String, Int)],
                                 jumpOperation: String,
                                 noOperation: String,
                                 index: Int): (Boolean, String, Int) = {
    val operationInfo = values(index)
    val status = operationInfo._1
    val operation = operationInfo._2
    val argument = operationInfo._3
    val changedOperation =
      if (operation == jumpOperation) noOperation
      else if (operation == noOperation) jumpOperation
      else operation
    (status, changedOperation, argument)
  }
}
