import scala.io.Source

object Day03 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"

        val slope: List[String] = Source.fromFile(filename).getLines.toList

        var treeCount = countTreesHit(slope, 0, 3, 0)
        println(s"Careful! Going down this slope you'd hit $treeCount trees!")
    }

    def countTreesHit(slope: List[String], xPos: Int, xInc: Int, treeCount: Int): Int = {
        if (slope.isEmpty) {
            treeCount
        } else {
            slope(0) match {
                case line: String if line(xPos) == '#' => {
                    val newPos = if (xPos + xInc >= slope(0).size) (xPos + xInc) - (slope(0).size) else xPos+xInc
                    countTreesHit(slope.drop(1), newPos, xInc, treeCount+1)
                }
                case line: String if line(xPos) == '.' => {
                    val newPos = if (xPos + xInc >= slope(0).size) (xPos + xInc) - (slope(0).size) else xPos+xInc
                    countTreesHit(slope.drop(1), newPos, xInc, treeCount)
                }
                case _ => throw new IllegalArgumentException("Something went wrong")
            }
        }
    }
}