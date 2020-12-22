import scala.io.Source

object Day03 {
    def main(args: Array[String]): Unit = {
        // MARK: General Setup
        val filename = "input.txt"
        val slope: List[String] = Source.fromFile(filename).getLines.toList
        
        // Part 1
        // val treeCount = countTreesHit(slope, 0, 3, 1, 0)
        // println(s"Careful! Going down this slope you'd hit $treeCount trees!")

        // Part 2
        val paths: List[(Int, Int)] = List((1, 1), (3, 1), (5, 1),(7, 1), (1, 2))
        var productOfTrees = 1

        for (path <- paths) {
            val treeCount = countTreesHit(slope, 0, path._1, path._2, 0)
            println(s"Careful! Going right ${path._1}, left ${path._2} you'd hit $treeCount trees!")
            productOfTrees = productOfTrees * treeCount
        }

        println(s"The product of all the trees you'd hit on thoe ${paths.size} slopes would be $productOfTrees")


    }

    def countTreesHit(slope: List[String], xPos: Int, xInc: Int, yInc: Int, treeCount: Int): Int = {
        if (slope.isEmpty) {
            treeCount
        } else {
            slope(0) match {
                case line: String if line(xPos) == '#' => {
                    val newPos = if (xPos + xInc >= slope(0).size) (xPos + xInc) - (slope(0).size) else xPos+xInc
                    countTreesHit(slope.drop(yInc), newPos, xInc, yInc, treeCount+1)
                }
                case line: String if line(xPos) == '.' => {
                    val newPos = if (xPos + xInc >= slope(0).size) (xPos + xInc) - (slope(0).size) else xPos+xInc
                    countTreesHit(slope.drop(yInc), newPos, xInc, yInc, treeCount)
                }
                case _ => throw new IllegalArgumentException("Something went wrong")
            }
        }
    }
}