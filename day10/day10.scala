import scala.io.Source

object Day10 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList.map(_.toInt).sortWith((_ < _))

        for (l<-lst) println(l)
        val jDiffs: (Int, Int) = findJoltDifferences(lst)
        val result: Int = jDiffs._1 * jDiffs._2 
        println(s"There is are ${jDiffs._1} 1-jolt differences and ${jDiffs._2} 3-jolt differences. (${jDiffs._1} * ${jDiffs._2} = $result) ")
    }

    def findJoltDifferences(nums: List[Int], acc: (Int, Int) = (1, 1)): (Int, Int) = {
        if (nums.size <= 1) { 
            acc
        } else {
        val jDiff = nums(1) - nums(0)
            jDiff match {
                case 1 => findJoltDifferences(nums.tail, (acc._1+1, acc._2))
                case 2 => findJoltDifferences(nums.tail, (acc._1, acc._2))
                case 3 => findJoltDifferences(nums.tail, (acc._1, acc._2+1))
                case _ => throw new IllegalArgumentException("Something went wrong...") 
            }
        }
    }
}