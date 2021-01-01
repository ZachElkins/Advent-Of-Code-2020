import scala.io.Source

object Day10 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        // val lst0 = Source.fromFile(filename).getLines.toList.map(_.toInt).sortWith((_ > _))

        val lst0 = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3).map(_.toInt).sortWith(_ < _)
        // val lst0 = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4).map(_.toInt).sortWith(_ < _)

        val lst = List(0) ++ lst0 ++ List(lst0.last+3)
        println(lst)
        // for (l<-lst) println(l)

        val jDiffs: (Int, Int) = findJoltDifferences(lst)
        val result: Int = jDiffs._1 * jDiffs._2 
        println(s"There is are ${jDiffs._1} 1-jolt differences and ${jDiffs._2} 3-jolt differences. (${jDiffs._1} * ${jDiffs._2} = $result) ")

        val totalArrangments: Long = findAllArrangments(lst)
        println(totalArrangments)
    }

    def findJoltDifferences(nums: List[Int], acc: (Int, Int) = (0, 0)): (Int, Int) = {
        if (nums.size <= 1) { 
            acc
        } else {
        val jDiff = nums(1) - nums(0)
            jDiff match {
                case 1 => findJoltDifferences(nums.tail, (acc._1+1, acc._2))
                case 3 => findJoltDifferences(nums.tail, (acc._1, acc._2+1))
                case _ => findJoltDifferences(nums.tail, (acc._1, acc._2))
            }
        }
    }

    def findAllArrangments(nums: List[Int]): Long = {
        var count: Long = 1
        for (i <- 1 until nums.size-1) {
            if (nums(i+1) - nums(i-1) <= 3) {
                val newNums = nums.slice(0, i) ++ nums.slice(i+1, nums.size)
                count = count + findAllArrangments(newNums)
            }
        }
        count
    }

    // def findAllArrangments0(nums: List[Int], acc: Long = 0): Long = {
    //     // println(s"Testing ${acc}")
    //     if (nums.size <= 1) {//List.empty) {
    //         acc + 1
    //     } else {
    //         val numsToTest = if (nums.size <= 3) nums.size else 3
    //         var temp: Long = 0
    //         for (i <- 1 until numsToTest) {
    //             if(nums(0) - nums(1) <= 3) {
    //                 findAllArrangments(nums.drop(1), acc+1)
    //             }
    //             findAllArrangments(nums.drop(1), acc+1)
    //         }
    //         acc+1
    //     }

    //     // Start from the end

    //     // Look at all that match the 1-3 jolt difference rule
        
    //     // Add that to accumulator

    //     // Do the same thing for each other one
    // }
}