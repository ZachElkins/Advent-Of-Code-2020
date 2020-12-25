import scala.io.Source

object Day06 {
    def main(args: Array[String]): Unit = {

        // Mark: General Setup
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList

        // MARK: Part 1
        val sumOfData: Int = convertToSets(lst).foldLeft(0)( (acc, elt) => acc + elt.size )
        println(s"The sum of the data is: $sumOfData")

        // MARK: Part 2
        val total: Int = partitionData(lst).foldLeft(List(0))( (acc, elt) => {
            val matches: List[String] = elt.foldLeft(('a' to 'z' ).toList.map(_.toString))( (acc, elt) => {
                acc.intersect(elt)
            })
            acc++List(matches.size)
        }).sum

        println(s"The real sum is: $total")
    }

    def partitionData(data: List[String]): List[List[List[String]]] = {
        var ret: List[List[List[String]]] = List.empty
        var curr: List[List[String]] = List.empty
        for (line <- data) {
            if (line.size == 0)  {
                ret = ret++List(curr)//List(List(curr.mkString("").split("").toList))
                curr = List.empty
            } else {
                // println(s"adding ${List(line.split(" +")(0).split("").toList)}")
                curr = curr++List(line.split(" +")(0).split("").toList) //curr++List(line.split(" +").toList)
            }
        }
        return ret++List(curr)
    }

    def convertToSets(data: List[String]): List[Set[String]] = {
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