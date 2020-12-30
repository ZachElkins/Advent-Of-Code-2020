import scala.io.Source

object Day11 {
    def main(args: Array[String]): Unit = {
        // MARK: General Setup
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList.map(_.split("").toList).toList

        // MARK: Part 1
        val result = musicalChairs(listToRay2D(lst), step _)
        val occupiedSeats = result.foldLeft(0)((acc, elt) => { acc + elt.foldLeft(0)((acc, elt) => { if (elt == "#") acc + 1 else acc }) })
        println(s"There are $occupiedSeats occupied seats!")

        // MARK: Part 2
        println("Changing Step Function For Part 2...")
        val result2 = musicalChairs(listToRay2D(lst), stepDeep _)

        val occupiedSeats2 = result2.foldLeft(0)((acc, elt) => { acc + elt.foldLeft(0)((acc, elt) => { if (elt == "#") acc + 1 else acc }) })
        println(s"There are $occupiedSeats2 occupied seats!")
    }

    def musicalChairs(grid: Array[Array[String]], stepFunc: Array[Array[String]] => Array[Array[String]], counter: Int = 1): Array[Array[String]] = {
        val oldGridCode = rayToString2D(grid)
        val newGrid = stepFunc(grid)
        val newGridCode = rayToString2D(newGrid)
        if (newGridCode == oldGridCode) {
            println(s"The seats stabilized after $counter changes!")
            grid
        } else {
            musicalChairs(newGrid, stepFunc, counter+1)
        }
    }

    // For Debugging
    def musicalChairsStep(grid: Array[Array[String]], stepFunc: Array[Array[String]] => Array[Array[String]], steps: Int = 1, counter: Int = 1): Array[Array[String]] = {
        if (counter > steps) {
            println(s"Finished after $steps steps!")
            grid
        } else {
            val oldGridCode = rayToString2D(grid)
            val newGrid = stepFunc(grid)
            val newGridCode = rayToString2D(newGrid)
            if (newGridCode == oldGridCode) {
                println(s"The seats stabilized after $counter changes!")
                grid
            } else {
                musicalChairsStep(newGrid, stepFunc, steps, counter+1)
            }
        }
    }

    def stepDeep(grid: Array[Array[String]]): Array[Array[String]] = {
        val oldGrid = rayToList2D(grid)
        var updatedGrid: Array[Array[String]] = grid
        for (x <- 0 until oldGrid.size) {
            for (y <- 0 until oldGrid(0).size) {
                var fullSeats: Int = 0

                // Not Left edge, check seat to the left
                if (x > 0) {
                    var flag = false
                    for (ix <- x-1 to 0 by -1) {
                        if (!flag) {
                            oldGrid(ix)(y) match {
                                case "#" => {
                                    fullSeats = fullSeats + 1
                                    flag = true
                                }
                                case "L" => {
                                    flag = true
                                }
                                case _ => {}
                            }
                        }
                    }
                    // AND Not bottom edge, check bottom left
                    if (y > 0) {
                        var flag = false
                        for (i <- 1 to x) {
                            if (!flag && y-i >= 0) {
                                oldGrid(x-i)(y-i) match {
                                    case "#" => {
                                        fullSeats = fullSeats + 1
                                        flag = true
                                    }
                                    case "L" => {
                                        flag = true
                                    }
                                    case _ => {}
                                }
                            }
                        }
                    }
                }

                // Not Right edge, check seat to the right
                if (x < oldGrid.size-1) {
                    var flag = false
                    for (ix <- x+1 until grid.size) {
                        if (!flag) {
                            oldGrid(ix)(y) match {
                                case "#" => {
                                    fullSeats = fullSeats + 1
                                    flag = true
                                }
                                case "L" => {
                                    flag = true
                                }
                                case _ => {}
                            }
                        }
                    }
                    // AND Not top edge, check top right
                    if (y < oldGrid(0).size-1) {
                        var flag = false
                        for (i <- 1 until (grid.size - x)) {
                            if (!flag && y+i < grid(0).size) {
                                oldGrid(x+i)(y+i) match {
                                    case "#" => {
                                        fullSeats = fullSeats + 1
                                        flag = true
                                    }
                                    case "L" => {
                                        flag = true
                                    }
                                    case _ => {}
                                }
                            }
                        }
                    }
                }

                // Not bottom edge, check seat below
                if (y > 0) {
                    var flag = false
                    for (iy <- y-1 to 0 by -1) {
                        if (!flag) {
                            oldGrid(x)(iy) match {
                                case "#" => {
                                    fullSeats = fullSeats + 1
                                    flag = true
                                }
                                case "L" => {
                                    flag = true
                                }
                                case _ => {}
                            }
                        }
                    }
                    // AND Not Right edge, check bottom right
                    if (x < oldGrid.size-1) {
                        var flag = false
                        for (i <- 1 until (grid.size - x))  {
                            if (!flag && y-i >= 0) {
                                oldGrid(x+i)(y-i) match {
                                    case "#" => {
                                        fullSeats = fullSeats + 1
                                        flag = true
                                    }
                                    case "L" => {
                                        flag = true
                                    }
                                    case _ => {}
                                }
                            }
                        }
                    }
                }
                // Not top edge, check seat above
                if (y < oldGrid(0).size-1) {
                    var flag = false
                    for (iy <- y+1 until grid(0).size) {
                        if (!flag) {
                            oldGrid(x)(iy) match {
                                case "#" => {
                                    fullSeats = fullSeats + 1
                                    flag = true
                                }
                                case "L" => {
                                    flag = true
                                }
                                case _ => {}
                            }
                        }
                    }
                    // AND Not Left edge, check seat to the top right
                    if (x > 0) {
                        var flag = false
                        for (i <- 1 to x) {
                            if (!flag && y+i < grid(0).size) {
                                oldGrid(x-i)(y+i) match {
                                    case "#" => {
                                        fullSeats = fullSeats + 1
                                        flag = true
                                    }
                                    case "L" => {
                                        flag = true
                                    }
                                    case _ => {}
                                }
                            }
                        }
                    }
                }

                // Update Grid
                oldGrid(x)(y) match {
                    case "L" => if (fullSeats == 0 ) updatedGrid(x)(y) = "#"
                    case "#" => if (fullSeats >= 5 ) updatedGrid(x)(y) = "L"
                    case _ => {}
                }
            }
        }
        // println(updatedGrid(0)(0), grid(0)(0))
        updatedGrid
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

                // print(grid(x)(y))
            }
            // print("\n")
        }
        // println(updatedGrid(0)(0), grid(0)(0))
        updatedGrid
    }

    // The array and list swapping is my solution to pass-by-reference causing issues
    def rayToString2D(ray: Array[Array[String]]): String = { ray.foldLeft("")((acc, elt) => { acc.concat(elt.foldLeft("")((acc2, elt2) => { acc2.concat(elt2) })) }) }
    def rayToList2D(ray: Array[Array[String]]): List[List[String]] = { ray.map(_.toList).toList }
    def listToRay2D(lst: List[List[String]]): Array[Array[String]] = { lst.map(_.toArray).toArray }
}