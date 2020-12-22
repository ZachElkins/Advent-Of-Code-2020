import scala.io.Source
import scala.util.matching.Regex

object Day02 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val regex = raw"(\d*)[\-](\d*)[\s]([a-z])[\:][\s]([a-z]*)".r
        val lst: List[String] = Source.fromFile(filename).getLines.toList

        // MARK: Part 1
        var valid = findValidPasswords(lst, regex, List.empty)
        println(s"The following ${valid.size} passwords are valud: ")
        println(valid)
    }

    def findValidPasswords(all: List[String], regex: Regex, valid: List[String]): List[String] = {

        if (all.isEmpty) {
            valid
        } else {
            all(0) match {
                case regex(min, max, char, password) => {
                    val countOfChar = password.count(_.toString == char)
                    if (countOfChar >= min.toInt && countOfChar <= max.toInt) {
                        val updatedList: List[String] = valid ++ List(password)
                        findValidPasswords(all.drop(1), regex, updatedList)
                    } else {
                        findValidPasswords(all.drop(1), regex, valid)
                    }
                }
                case _ => throw new IllegalArgumentException("Pattern not matched properly")
            }
        }

    }
}

