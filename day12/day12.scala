import scala.io.Source

object Day12 {
    def main(args: Array[String]): Unit = {
        // MARK: General Setup
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList
        // val lst = List(
        //     "F10",
        //     "N3",
        //     "F7",
        //     "R90",
        //     "F11"
        // )

        // MARK: Part 1
        val instructions = lst.map(ins => (ins(0).toString, ins.substring(1, ins.size).toInt))
        val d = readInstructionsLiteral(instructions)
        println(s"The ship's Manhattan distance is ${(d._1 + d._2).abs}")
        
        // MARK: Part 2

        /*
            Incorrect attempts
             - 51039
             - 48623
             - 14691
             - 4855
        */
        
        println("Adjusting for new instructions...")
        
        val w = readInstructionsWaypoint(instructions)
        println(s"The ship's Manhattan distance is ${(w._1 + w._2).abs}")
    }

    def readInstructionsWaypoint(instructions: List[(String, Int)], acc: (Int, Int) = (0, 0), wayPoint: (Int, Int) = (10, 1)): (Int, Int) = {
        if (instructions == List.empty) {
            acc
        } else {
            instructions(0) match {
                case (d: String, m: Int) => {
                    d match {
                        case "N" => readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1, wayPoint._2+m))
                        case "E" => readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1+m, wayPoint._2))
                        case "S" => readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1, wayPoint._2-m))
                        case "W" => readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1-m, wayPoint._2))
                        case "R" => m match {
                            case 90  => if (signsMatch(wayPoint)) readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1, wayPoint._2 * -1)) else readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1 * -1, wayPoint._2))
                            case 180 => readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1 * -1, wayPoint._2 * -1))
                            case 270 => if (signsMatch(wayPoint)) readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1 * -1, wayPoint._2)) else readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1, wayPoint._2 * -1))
                        }
                        case "L" => m match {
                            case 90  => if (signsMatch(wayPoint)) readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1 * -1, wayPoint._2)) else readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1, wayPoint._2 * -1))
                            case 180 => readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1 * -1, wayPoint._2 * -1))
                            case 270 => if (signsMatch(wayPoint)) readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1, wayPoint._2 * -1)) else readInstructionsWaypoint(instructions.tail, acc, (wayPoint._1 * -1, wayPoint._2))
                        }
                        case "F" => readInstructionsWaypoint(instructions.tail, (acc._1+(m*wayPoint._1), acc._2+(m*wayPoint._2)), wayPoint)
                    }
                }
            }
        }
    }

    def readInstructionsLiteral(instructions: List[(String, Int)], acc: (Int, Int) = (0, 0), currentDirection: Int = 2): (Int, Int) = {
        if (instructions == List.empty) {
            acc
        } else {
            instructions(0) match {
                case (d: String, m: Int) => {
                    d match {
                        case "N" => readInstructionsLiteral(instructions.tail, (acc._1, acc._2+m), currentDirection)
                        case "E" => readInstructionsLiteral(instructions.tail, (acc._1+m, acc._2), currentDirection)
                        case "S" => readInstructionsLiteral(instructions.tail, (acc._1, acc._2-m), currentDirection)
                        case "W" => readInstructionsLiteral(instructions.tail, (acc._1-m, acc._2), currentDirection)
                        case "R" => m match {
                            case 90  => readInstructionsLiteral(instructions.tail, acc, if(currentDirection+1 > 4) currentDirection+1-4 else currentDirection+1)
                            case 180 => readInstructionsLiteral(instructions.tail, acc, if(currentDirection+2 > 4) currentDirection+2-4 else currentDirection+2)
                            case 270 => readInstructionsLiteral(instructions.tail, acc, if(currentDirection+3 > 4) currentDirection+3-4 else currentDirection+3)
                        }
                        case "L" => m match {
                            case 90  => readInstructionsLiteral(instructions.tail, acc, if(currentDirection-1 < 1) 4+(currentDirection-1) else currentDirection-1)
                            case 180 => readInstructionsLiteral(instructions.tail, acc, if(currentDirection-2 < 1) 4+(currentDirection-2) else currentDirection-2)
                            case 270 => readInstructionsLiteral(instructions.tail, acc, if(currentDirection-3 < 1) 4+(currentDirection-3) else currentDirection-3)
                        }
                        case "F" => currentDirection match {
                            case 1 => readInstructionsLiteral(instructions.tail, (acc._1, acc._2+m), currentDirection)
                            case 2 => readInstructionsLiteral(instructions.tail, (acc._1+m, acc._2), currentDirection)
                            case 3 => readInstructionsLiteral(instructions.tail, (acc._1, acc._2-m), currentDirection)
                            case 4 => readInstructionsLiteral(instructions.tail, (acc._1-m, acc._2), currentDirection)
                        }
                    }
                }
            }       
        }
    }

    def signsMatch(nums: (Int, Int)): Boolean = { (nums._1 > 0 && nums._2 > 0) || (nums._1 < 0 && nums._1 < 0) }
}