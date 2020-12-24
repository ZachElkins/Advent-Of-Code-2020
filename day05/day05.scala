import scala.io.Source

object Day05 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst: List[String] = Source.fromFile(filename).getLines.toList
        val rowRange: (Int, Int) = (0, 127)
        val colRange: (Int, Int) = (0, 7)
        println(s"Highest seat ID is ${findMax(lst, rowRange, colRange, 0)}")
    }

    def findMax(codes: List[String], rowRange: (Int, Int), colRange: (Int, Int), max: Int): Int = {
        if(codes.isEmpty) {
            max
        } else {
            val rowCode = codes(0).substring(0, 7)
            val colCode = codes(0).substring(7, 10)
            val sID = seatId(findRow(rowCode, rowRange), findCol(colCode, colRange))
            if (sID > max) {
                findMax(codes.drop(1), rowRange, colRange, sID)  
            } else {
                findMax(codes.drop(1), rowRange, colRange, max)
            }
        }
    }

    def seatId(r: Int, c: Int): Int = r*8+c

    def findRow(str: String, range: (Int, Int)): Int = {
        if (str == "B") {
            range._2
        } else if (str == "F") {
            range._1
        } else {
            str.substring(0,1) match {
                case "F" => {
                    val newRange = (range._1, ((range._1+range._2)/2).toInt)
                    // println(s"F means to take the lower half, keeping rows ${newRange._1} through ${newRange._2}")
                    findRow(str.tail, newRange)
                }
                case "B" => {
                    val newRange = (((range._1+range._2)/2+1).toInt, range._2)
                    // println(s"B means to take the upper half, keeping rows ${newRange._1} through ${newRange._2}")
                    findRow(str.tail, newRange)
                }
                case _ => throw new IllegalArgumentException(s"rInput: ${str.substring(0,1)}")
            }
        }
    }


    def findCol(str: String, range: (Int, Int)): Int = {
        if (str == "R") {
            range._2
        } else if (str == "L") {
            range._1
        } else {
            str.substring(0,1) match {
                case "L" => {
                    val newRange = (range._1, ((range._1+range._2)/2).toInt)
                    // println(s"L means to take the upper half, keeping columns ${newRange._1} through ${newRange._2}")
                    findCol(str.tail, newRange)
                }
                case "R" => {
                    val newRange = (((range._1+range._2)/2+1).toInt, range._2)
                    // println(s"R means to take the lower half, keeping columns ${newRange._1} through ${newRange._2}")
                    findCol(str.tail, newRange)
                }
                case _ => throw new IllegalArgumentException(s"cInput: ${str.substring(0,1)}")
            }
        }
    }
}