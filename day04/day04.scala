import scala.io.Source
import scala.util.matching.Regex

object Day04 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val regex = raw"([\w]*\:[\w\S]*\s)*".r
        val lst: List[String] = Source.fromFile(filename).getLines.toList
        val requiredFields: List[String] = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

        // This is the worst line of code I have ever written... I'm sorry :( => Doesnt even matter becuase the oringal function I wrote doesnt
        // val tidyList = partitionData(lst.take(8), List.empty, List.empty).map(data => regex.findAllIn(data).toList)//.filter(_.nonEmpty)(0).split(" +").toList)//.map( _.map( _.substring(0, 3))).map(_.filter(_ != "cid"))
        
        val tidyList1 = partitionData(lst).map(data => data(0).split(" +").toList)//.map( _.map( _.substring(0, 3))).map(_.filter(_ != "cid"))
        val tidyList = partitionData(lst).map(_.map(_.substring(0,3))).map(_.filter(_!="cid"))
        var count = 0

        for (line <- tidyList) if(line.size == 7) count = count + 1

        println(s"There are $count 'valid' passports!")
        
    }
    def partitionData(data: List[String]): List[List[String]] = {
        var ret: List[List[String]] = List.empty
        var curr: List[String] = List.empty
        for (line <- data) {
            if (line.size == 0)  {
                ret = ret++List(curr)
                curr = List.empty
            } else {
                curr = curr++line.split(" +").toList
            }
        }
        return ret++List(curr)
    }

    /*

    // Failed first recursive solution. For some reason it is always missing the last item on each "passport"
    // Any help would be apreciated...

    def partitionData(data: List[String], acc: List[String], ret: List[String]): List[String] = {
        // Return ret if data is empty
        if (data.isEmpty) {
            return ret
        } else {
            data(0) match {
                // Check if data(0) is a blank line | recurse with acc = list.empty and data.drop(1) and ret++acc
                case x: String if x == "" => {
                    println(s"skipping line: ${data(0)}")
                    // partitionData(data.drop(1), List.empty, ret++List(acc.mkString(" ")))
                    partitionData(data.drop(1), List.empty, ret++(acc))
                }
                // Otherwise add line to acc and recurese with new acc and data.drop(1)
                case x: String => {
                    println(s"Appending line: ${data(0)}")
                    partitionData(data.drop(1), acc++List(x), ret)
                }
                case _ => throw new IllegalArgumentException("Something went wrong")
            }
        }
    }

    // Why bother at this point with the recursion if I cound't get the other one to work :\
    def validPassportCount(passports: List[List[String]], reqFields: List[String], acc: Int): Int = {
        if (passports.isEmpty) {
            return acc
        } else {
            if (reqFields.size == passports(0).size) {
                validPassportCount(passports.drop(1), reqFields, acc + 1)
            } else {
                validPassportCount(passports.drop(1), reqFields, acc)
            }
        }
    }

    */
}