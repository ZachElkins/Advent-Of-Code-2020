import scala.io.Source

object Day09 {
    def main(args: Array[String]): Unit = {
        // MARK: General Setup
        val filename = "input.txt"
        val lst: List[Long] = Source.fromFile(filename).getLines.toList.map(_.toLong)
        
        // MARK: Part 1
        val preambleLength = 25
        val invalidNum = findInvalidNumber(lst, preambleLength)
        println(s"The invalid number is $invalidNum")

        // MARK: Part 2
        val numbers = findNumbersAddingToSum(lst, invalidNum, 0, 2).sortWith(_ > _)
        val sum = numbers.head + numbers.last
        println(s"$numbers sum to $invalidNum. The highest and lowest sum to $sum")
        
    }

    def findInvalidNumber(numbers: List[Long], preambleLength: Int): Long = {
        val targetNum = numbers(preambleLength)
        var flag: Boolean = false
        val slice = numbers.slice(0, preambleLength)

        for (num1 <- slice)
            for (num2 <- slice)
                if (num1 != num2 && num1+num2 == targetNum)
                    flag = true
                    
        if (flag) findInvalidNumber(numbers.drop(1), preambleLength)
        else targetNum
    }

    def findNumbersAddingToSum(numbers: List[Long], target: Long, startIndex: Int, numbersToCheck: Int): List[Long] = {
        if(startIndex + numbersToCheck >= numbers.size-1) {
            findNumbersAddingToSum(numbers, target, 0, numbersToCheck+1)
        } else {
            val slice = numbers.slice(startIndex, startIndex + numbersToCheck)
            if (slice.sum == target) slice
            else findNumbersAddingToSum(numbers, target, startIndex+1, numbersToCheck)
        }
    }
}