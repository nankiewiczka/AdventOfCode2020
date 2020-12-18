import scala.io.Source
import scala.util.Using

object DayX {
  def main(args: Array[String]) = {
    Using.resource(Source.fromFile("src/main/data/day5.txt")) {
      source =>
        val values = source.getLines().toList

    }
  }
}
