import scala.io.Source
// import sbt._

object Day01 {
    def main(args: Array[String]): Unit = {

        val filename = "input.txt"
        // val lines = Source.fromFile(filename).getLines
        val target = 2020

        val lstStr: List[String] = Source.fromFile(filename).getLines.toList
        val lstInt: List[Int] = lstStr.map(_.toInt)

        val nums = findTwoSummingToTarget( lstInt, target )
        val solution = nums._1 * nums._2

        if (nums._1 == -1) println("No solution found")
        else println(s"Solution found! ${nums._1} + ${nums._2} = $target, so ${nums._1} * ${nums._2} = $solution")

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
}
