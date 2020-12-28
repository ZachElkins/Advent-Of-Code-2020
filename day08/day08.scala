import scala.io.Source
import scala.util.matching.Regex

object Day08 {
    def main(args: Array[String]): Unit = {
        // MARK: General Setup
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList

        // MARK: Part 1
        val instructions: List[(String, Int, Boolean)] = lst.map(ins => (ins.substring(0, 3), ins.substring(4, ins.size).toInt, false))
        // for (b <- instructions) println(b)
        val result = readInstructions(instructions, 0, 0)
        println(s"The result of the instructions is $result.")
    }
    
    def readInstructions(instructions: List[(String, Int, Boolean)], index: Int, acc: Int): Int = {
        instructions(index) match {
            case (i: String, v: Int, f: Boolean) => {
                if (f) {
                    acc
                } else {
                    i match {
                        case "jmp" => {
                            val flaggedInstructions = instructions.slice(0, index) ++ List((i, v, true)) ++ instructions.slice(index+1, instructions.size)
                            readInstructions(flaggedInstructions, index+v, acc)
                        }
                        case "acc" => {
                            val flaggedInstructions = instructions.slice(0, index) ++ List((i, v, true)) ++ instructions.slice(index+1, instructions.size)
                            readInstructions(flaggedInstructions, index+1, acc+v)
                        }
                        case "nop" => {
                            val flaggedInstructions = instructions.slice(0, index) ++ List((i, v, true)) ++ instructions.slice(index+1, instructions.size)
                            readInstructions(flaggedInstructions, index+1, acc)
                        }
                        case _ => throw new IllegalArgumentException("Something went wrong...")
                    }
                }
            }
            case _ => throw new IllegalArgumentException("Something went wrong...")
        }
    }
}