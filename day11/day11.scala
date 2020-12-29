import scala.io.Source

object Day11 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList.map(_.split("")).toArray

        val result = musicalChairs(lst)

        val occupiedSeats = result.foldLeft(0)((acc, elt) => { acc + elt.foldLeft(0)((acc, elt) => { if (elt == "#") acc + 1 else acc }) })
        println(s"There are $occupiedSeats occupied seats!")
    }

    def musicalChairs(grid: Array[Array[String]], counter: Int = 1): Array[Array[String]] = {
        val oldGridCode = rayToString2D(grid)
        val newGrid = step(grid)
        val newGridCode = rayToString2D(newGrid)
        if (newGridCode == oldGridCode) {
            println(s"The seats stabilized after $counter changes!")
            grid
        } else {
            musicalChairs(newGrid, counter+1)
        }
    }

    def step(grid: Array[Array[String]]): Array[Array[String]] = {
        val oldGrid = rayToList2D(grid)
        var updatedGrid: Array[Array[String]] = grid
        for (x <- 0 until oldGrid.size) {
            for (y <- 0 until oldGrid(0).size) {
                var fullSeats: Int = 0

                // Not Left edge, check seat to the left
                if (x > 0) {
                    oldGrid(x-1)(y) match {
                        case "#" => fullSeats = fullSeats + 1
                        case _ => {}
                    }
                    // AND Not bottom edge, check bottom left
                    if (y > 0) {
                        oldGrid(x-1)(y-1) match {
                            case "#" => fullSeats = fullSeats + 1
                            case _ => {}
                        }
                    }
                }

                // Not Right edge, check seat to the left
                if (x < oldGrid.size-1) {
                    oldGrid(x+1)(y) match {
                        case "#" => fullSeats = fullSeats + 1
                        case _ => {}
                    }

                    // AND Not top edge, check top right
                    if (y < oldGrid(0).size-1) {
                        oldGrid(x+1)(y+1) match {
                            case "#" => fullSeats = fullSeats + 1
                            case _ => {}
                        }
                    }
                }

                // Not bottom edge, check seat below
                if (y > 0) {
                    oldGrid(x)(y-1) match {
                        case "#" => fullSeats = fullSeats + 1
                        case _ => {}
                    }

                    // AND Not Right edge, check top left
                    if (x < oldGrid.size-1) {
                        oldGrid(x+1)(y-1) match {
                            case "#" => fullSeats = fullSeats + 1
                            case _ => {}
                        }
                    }
                }

                // Not top edge, check seat above
                if (y < oldGrid(0).size-1) {
                    oldGrid(x)(y+1) match {
                        case "#" => fullSeats = fullSeats + 1
                        case _ => {}
                    }

                    // AND Not Left edge, check seat to the bottom right
                    if (x > 0) {
                        oldGrid(x-1)(y+1) match {
                            case "#" => fullSeats = fullSeats + 1
                            case _ => {}
                        }
                    }
                }

                // Update Grid
                oldGrid(x)(y) match {
                    case "L" => if (fullSeats == 0 ) updatedGrid(x)(y) = "#"
                    case "#" => if (fullSeats >= 4 ) updatedGrid(x)(y) = "L"
                    case _ => {}
                }
            }
        }
        updatedGrid
    }

    def rayToString2D(ray: Array[Array[String]]): String = {
        ray.foldLeft("")((acc, elt) => {
            acc.concat(elt.foldLeft("")((acc2, elt2) => {
                acc2.concat(elt2)
            }))
        })
    }

    def rayToList2D(ray: Array[Array[String]]): List[List[String]] = { ray.map(_.toList).toList }
}