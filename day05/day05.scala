import scala.io.Source

object Day05 {
    def main(args: Array[String]): Unit = {
        // MARK: General Setup
        val filename = "input.txt"
        val lst: List[String] = Source.fromFile(filename).getLines.toList
        val rowRange: (Int, Int) = (0, 127)
        val colRange: (Int, Int) = (0, 7)

        // MARK: Part 1
        println(s"Highest seat ID is ${findMax(lst, rowRange, colRange, 0)}")

        // MARKA: Part 2
        // var seats = getAllSeats(lst, rowRange, colRange, List.empty)
        // for (seat <- seats) println(seat)
        var sIDs = getAllIDs(lst, rowRange, colRange, List.empty)
        val missingSeat = findSeat(sIDs, 0)
        // for (sID <- sIDs) println(sID)
        println(s"Seat found! Seat ID: ${missingSeat}")
    }
    
    def findSeat(sIDs: List[Int], index: Int): Int = {
        if( sIDs(index)+2 == sIDs(index+1)) {
            (sIDs(index)+1)
        } else {
            findSeat(sIDs, index+1)
        }
    }

    def sortSeatIDs(s1: Int, s2: Int): Boolean = {
        s1 <= s2
    }

    def getAllIDs(codes: List[String], rowRange: (Int, Int), colRange: (Int, Int), sIDs: List[Int]): List[Int] = {
        if (codes.isEmpty) {
            sIDs.sortWith(sortSeatIDs)
        } else {
            val rowCode = codes(0).substring(0, 7)
            val colCode = codes(0).substring(7, 10)
            val sID = seatId(findRow(rowCode, rowRange), findCol(colCode, colRange))
            getAllIDs(codes.drop(1), rowRange, colRange, sIDs++List(sID))
        }
    }

    // def sortSeats(s1: (Int, Int), s2: (Int, Int)): Boolean = {
    //     if (s1._1 == s2._1) {
    //         s1._2 <= s2._2
    //     } else {
    //         s1._1 <= s2._1
    //     }
    // }

    // def getAllSeats(codes: List[String], rowRange: (Int, Int), colRange: (Int, Int), seats: List[(Int, Int)]): List[(Int, Int)] = {
    //     if (codes.isEmpty) {
    //         codes.sortWith(sortSeats)
    //     } else {
    //         val rowCode = codes(0).substring(0, 7)
    //         val colCode = codes(0).substring(7, 10)
    //         getAllSeats(codes.drop(1), rowRange, colRange, seats++List((findRow(rowCode, rowRange), findCol(colCode, colRange))))
    //     }
    // }

    def findMax(codes: List[String], rowRange: (Int, Int), colRange: (Int, Int), max: Int): Int = {
        if (codes.isEmpty) {
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