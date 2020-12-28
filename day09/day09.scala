import scala.io.Source
import scala.util.matching.Regex

object Day09 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst: List[Long] = Source.fromFile(filename).getLines.toList.map(_.toLong)
        
        val preambleLength = 25

        val invalidNum = findInvalidNumber(lst, preambleLength)
        println(s"The invalid number is $invalidNum")

    }

    def findInvalidNumber(numbers: List[Long], preambleLength: Int): Long = {
        val targetNum = numbers(preambleLength)
        var flag: Boolean = false
        val slice = numbers.slice(0, preambleLength)

        for (num1 <- slice)
            for (num2 <- slice)
                if (num1 != num2 && num1+num2 == targetNum)
                    flag = true
                    
        if (flag) findFirstNotSum(numbers.drop(1), preambleLength)
        else targetNum
    }
}