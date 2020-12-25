import scala.io.Source

object Day06 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList

        val sumOfData: Int = partitionData(lst).foldLeft(0)( (acc, elt) => acc + elt.size )
        println(s"The sum of the data is: $sumOfData")
    }

    def partitionData(data: List[String]): List[Set[String]] = {
        var ret: List[Set[String]] = List.empty
        var curr: List[String] = List.empty
        for (line <- data) {
            if (line.size == 0)  {
                ret = ret++List(curr.mkString("").split("").toSet)
                curr = List.empty
            } else {
                curr = curr++line.split(" +").toList
            }
        }
        return ret++List(curr.mkString("").split("").toSet)
    }
}