import scala.io.Source
import sbt._

object Day01 {
    def main(args: Array[String]): Unit = {

        val filename = "input.txt"
        // val lines = Source.fromFile(filename).getLines
        val target = 2020

        for (s1 <- Source.fromFile(filename).getLines) {

            val v1 = s1.toInt
            
            for (s2 <- Source.fromFile(filename).getLines) {

                val v2 = s2.toInt
                
                if (v1 + v2 == target) {
                    val solution = v1 * v2
                    println(s"Solution found! $v1 + $v2 = $target, so $v1 * $v2 = $solution")
                }
            }
        }
    }
}
