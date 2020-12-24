import scala.io.Source
import scala.util.matching.Regex

object Day04 {
    def main(args: Array[String]): Unit = {

        // MARK: General Setup
        val filename = "input.txt"
        val regex = raw"([\w]*\:[\w\S]*\s)*".r
        val lst: List[String] = Source.fromFile(filename).getLines.toList
        val requiredFields: List[String] = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")


        // MARK: Part 1
        // This is the worst line of code I have ever written... I'm sorry :( => Doesnt even matter becuase the oringal function I wrote doesnt
        // val tidyList = partitionData(lst.take(8), List.empty, List.empty).map(data => regex.findAllIn(data).toList)//.filter(_.nonEmpty)(0).split(" +").toList)//.map( _.map( _.substring(0, 3))).map(_.filter(_ != "cid"))
        val tidyList1 = partitionData(lst).map(_.map(_.substring(0,3))).map(_.filter(_!="cid"))

        var count = 0
        for (line <- tidyList1) if (line.diff(requiredFields) == List.empty) count = count + 1

        println(s"There are $count 'valid' passports... If we don't look too close")
        
        // MARK: Part 2
        val tidyList2 = partitionData(lst).map(_.filter(_.substring(0,3)!="cid")).filter(_.length != requiredFields.size)
        count = 0
        // for(line <- tidyList2) if (checkData(line)) count = count + 1
        for(line <- tidyList2) {
            if (checkData(line)) {
                count = count + 1
            } //else {
                // println(s"Bad: $line")
            // }
        }
        println(s"There are $count 'valid' passports... If we don't count cid")

    }

    def checkData(data: List[String]): Boolean = {
        if (data.isEmpty) return true

        data(0).substring(0,3) match {
            case "byr" => inRange( 1920, 2002, data(0).split(":")(1).toInt)
            case "iyr" => inRange( 2010, 2020, data(0).split(":")(1).toInt)
            case "eyr" => inRange( 2020, 2030, data(0).split(":")(1).toInt)
            case "hgt" => data(0).split(":")(1) match {
                case y: String if y.takeRight(2) == "in" => {
                    val z: Int = y.substring(0, y.size-2).toInt
                    if (z >= 59 && z <= 76) {
                        checkData(data.drop(1))
                    } else {
                        false
                    }
                }
                case y: String if y.takeRight(2) == "cm" => {
                    val z: Int = y.substring(0, y.size-2).toInt
                    if (z >= 150 && z <= 193) {
                        checkData(data.drop(1))
                    } else {
                        false
                    }
                }
                case _ => false
            }
            case "hcl" => {
                val reg = raw"(#{1}[A-fa-f\d]{6})".r
                data(0).split(":")(1) match {
                    case reg(_) => checkData(data.drop(1))
                    case _ => false
                }
            }
            case "ecl" => data(0).split(":")(1) match {
                case "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
                case _ => {
                    false
                }
            }
            case x: String if x == "pid" => x.size == 9 && x.forall(Character.isDigit)
            case _ => false
        }
    }

    def inRange(min: Int, max: Int, test: Int): Boolean = (test >= min && test <= max)
    // def checkData(data: List[String]): Boolean = {
    //     if (data.isEmpty) return true

    //     data(0).substring(0,3) match {
    //         case x: String if x == "byr" => inRange( 1920, 2002, data(0).split(":")(1).toInt)
    //         case x: String if x == "iyr" => inRange( 2010, 2020, data(0).split(":")(1).toInt)
    //         case x: String if x == "eyr" => inRange( 2020, 2030, data(0).split(":")(1).toInt)
    //         case x: String if x == "hgt" => data(0).split(":")(1) match {
    //             case y: String if x.takeRight(2) == "in" => {
    //                 val z: Int = y.substring(0, y.size-2).toInt
    //                 z >= 59 && z <= 76
    //             }
    //             case y: String if x.takeRight(2) == "cm" => {
    //                 val z: Int = y.substring(0, y.size-2).toInt
    //                 z >= 150 && z <= 193
    //             }
    //             case _ => false
    //         }
    //         case x: String if x == "hcl" => {
    //             val reg = raw"#{1}[A-Za-z\d]{6}".r
    //             data(0).split(":") match {
    //                 case reg(_) => true
    //                 case _ => false
    //             }
    //         }
    //         case x: String if x == "ecl" => data(0).split(":")(1) match {
    //             case "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
    //             case _ => false
    //         }
    //         case x: String if x == "pid" => x.size == 9 && x.forall(Character.isDigit)
    //         case _ => false
    //     }
    // }
    // 
    // def inRange(min: Int, max: Int, test: Int): Boolean = test >= min && test <= max

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