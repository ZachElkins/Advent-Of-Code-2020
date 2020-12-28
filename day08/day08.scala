import scala.io.Source

object Day08 {
    def main(args: Array[String]): Unit = {

        println("Hang in there, this one might take a while to run...")

        // MARK: General Setup
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList

        // MARK: Part 1
        val instructions: List[(String, Int, Boolean)] = lst.map(ins => (ins.substring(0, 3), ins.substring(4, ins.size).toInt, false))
        val result = readInstructions(instructions, 0, 0)
        println(s"The result of the instructions is ${result._2}.")

        // MARK: Part 2
        val result2 = fixInstructions(instructions, 0)
        println(s"The result of the fixed instructions is $result2.")
        
    }

    def fixInstructions(instructions: List[(String, Int, Boolean)], index: Int): Int = {
        instructions(index) match {
            case (i: String, v: Int, f: Boolean) => i match {
                case "jmp" => {
                    val newInstructions = instructions.slice(0, index) ++ List(("nop", v, false)) ++ instructions.slice(index+1, instructions.size)
                    val res = readInstructions(newInstructions, 0, 0)
                    if (res._1 == 1) {
                        res._2
                    } else {
                        fixInstructions(instructions, index+1)
                    }
                }
                case "nop" => {
                    val newInstructions = instructions.slice(0, index) ++ List(("jmp", v, false)) ++ instructions.slice(index+1, instructions.size)
                    val res = readInstructions(newInstructions, 0, 0)
                    if (res._1 == 1) {
                        res._2
                    } else {
                        fixInstructions(instructions, index+1)
                    }
                }
                case "acc" => fixInstructions(instructions, index+1)
                case _ => throw new IllegalArgumentException("Something went wrong... (a)")
            }
            case _ => throw new IllegalArgumentException("Something went wrong... (b)")
        }
    }
    
    def readInstructions(instructions: List[(String, Int, Boolean)], index: Int, acc: Int): (Int, Int) = {
        if (index == instructions.size) {
            (1, acc)
        } else {
            instructions(index) match {
                case (i: String, v: Int, f: Boolean) => {
                    if (f) {
                        (0, acc)
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
}
