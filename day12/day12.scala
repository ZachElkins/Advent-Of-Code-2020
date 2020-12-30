import scala.io.Source

object Day12 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList
        // val lst = List(
        //     "F10",
        //     "N3",
        //     "F7",
        //     "R270",
        //     "F11",
        //     "W2",
        //     "S2"
        // )

        val instructions = lst.map(ins => (ins(0).toString, ins.substring(1, ins.size).toInt))
        val d = readInstructions(instructions)
        println(d)
        println(s"The ship's Manhattan distance is ${(d._1 + d._2).abs}")
    }

    def readInstructions(instructions: List[(String, Int)], acc: (Int, Int) = (0, 0), currentDirection: Int = 2): (Int, Int) = {
        if (instructions == List.empty) {
            acc
        } else {
            instructions(0) match {
                case (d: String, m: Int) => {
                    d match {
                        case "N" => readInstructions(instructions.tail, (acc._1, acc._2+m), currentDirection)
                        case "E" => readInstructions(instructions.tail, (acc._1+m, acc._2), currentDirection)
                        case "S" => readInstructions(instructions.tail, (acc._1, acc._2-m), currentDirection)
                        case "W" => readInstructions(instructions.tail, (acc._1-m, acc._2), currentDirection)
                        case "R" => m match {
                            case 90  => readInstructions(instructions.tail, acc, if(currentDirection+1 > 4) currentDirection+1-4 else currentDirection+1 )
                            case 180 => readInstructions(instructions.tail, acc, if(currentDirection+2 > 4) currentDirection+2-4 else currentDirection+2 )
                            case 270 => readInstructions(instructions.tail, acc, if(currentDirection+3 > 4) currentDirection+3-4 else currentDirection+3 )
                        }
                        case "L" => m match {
                            case 90  => readInstructions(instructions.tail, acc, if(currentDirection-1 < 1) 4+(currentDirection-1) else currentDirection-1 )
                            case 180 => readInstructions(instructions.tail, acc, if(currentDirection-2 < 1) 4+(currentDirection-2) else currentDirection-2 )
                            case 270 => readInstructions(instructions.tail, acc, if(currentDirection-3 < 1) 4+(currentDirection-3) else currentDirection-3 )
                        }
                        case "F" => currentDirection match {
                            case 1 => readInstructions(instructions.tail, (acc._1, acc._2+m), currentDirection)
                            case 2 => readInstructions(instructions.tail, (acc._1+m, acc._2), currentDirection)
                            case 3 => readInstructions(instructions.tail, (acc._1, acc._2-m), currentDirection)
                            case 4 => readInstructions(instructions.tail, (acc._1-m, acc._2), currentDirection)
                        }
                    }
                }
            }       
        }
    }
}