import scala.io.Source

object Day01 {
    def main(args: Array[String]): Unit = {

        // MARK: General Setup
        val filename = "input.txt"
        
        val target = 2020

        val lstStr: List[String] = Source.fromFile(filename).getLines.toList
        val lstInt: List[Int] = lstStr.map(_.toInt)

        // MARK: Part 1
        val twoNums = findTwoSummingToTarget( lstInt, target )
        val p1Solution = twoNums._1 * twoNums._2

        if (twoNums._1 == -1) println("No solution found for part 1")
        else println(s"Solution found! ${twoNums._1} + ${twoNums._2} = $target, so ${twoNums._1} * ${twoNums._2} = $p1Solution")

        // MARK: Part 2
        val threeNums = findThreeSummingToTarget( lstInt, target )
        val p2Solution = threeNums._1 * threeNums._2 * threeNums._3

        if (threeNums._1 == -1) println("No solution found for part 2")
        else println(s"Solution found! ${threeNums._1} + ${threeNums._2} + ${threeNums._3} = $target, so ${threeNums._1} * ${threeNums._2} * ${threeNums._3} = $p2Solution")

    }

    def findTwoSummingToTarget(lst: List[Int], target: Int): (Int, Int) = {
        for (v1 <- lst) {
            for (v2 <- lst) {
                if (v1 + v2 == target) {
                    return (v1, v2)
                }
            }
        }
        (-1, -1)
    }

    def findThreeSummingToTarget(lst: List[Int], target: Int): (Int, Int, Int) = {
        for (v1 <- lst) {
            for (v2 <- lst) {
                for (v3 <- lst) {
                    if (v1 + v2 + v3 == target) {
                        return (v1, v2, v3)
                    }
                }
            }
        }
        (-1, -1, -1)
    }
}
