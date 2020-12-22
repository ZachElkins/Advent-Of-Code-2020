import scala.io.Source
import scala.util.matching.Regex

object Day02 {
    def main(args: Array[String]): Unit = {
        
        // MARK: General Setup
        val filename = "input.txt"
        val regex = raw"(\d*)[\-](\d*)[\s]([a-z])[\:][\s]([a-z]*)".r
        val lst: List[String] = Source.fromFile(filename).getLines.toList

        // MARK: Part 1
        var validSledPasswords = findValidPasswordsSled(lst, regex, List.empty)
        println(s"There are ${validSledPasswords.size} valid Sled passwords")
        // println(validSledPasswords)

        // MARK: Part 1
        var validToboggonPassowords = findValidPasswordsToboggon(lst, regex, List.empty)
        println(s"There are ${validToboggonPassowords.size} valid Toboggon passwords.")
        // println(validToboggonPassowords)
    }

    def findValidPasswordsSled(all: List[String], regex: Regex, valid: List[String]): List[String] = {

        if (all.isEmpty) {
            valid
        } else {
            all(0) match {
                case regex(min, max, char, password) => {
                    val countOfChar = password.count(_.toString == char)
                    if (countOfChar >= min.toInt && countOfChar <= max.toInt) {
                        val updatedList: List[String] = valid ++ List(password)
                        findValidPasswordsSled(all.drop(1), regex, updatedList)
                    } else {
                        findValidPasswordsSled(all.drop(1), regex, valid)
                    }
                }
                case _ => throw new IllegalArgumentException("Pattern not matched properly")
            }
        }

    }


    def findValidPasswordsToboggon(all: List[String], regex: Regex, valid: List[String]): List[String] = {

        if (all.isEmpty) {
            valid
        } else {
            all(0) match {
                case regex(i1, i2, char, password) => {
                    if ( ( password(i1.toInt-1).toString == char || password(i2.toInt-1).toString == char ) && !( password(i1.toInt-1).toString == char && password(i2.toInt-1).toString == char ) ) {
                        val updatedList: List[String] = valid ++ List(password)
                        findValidPasswordsToboggon(all.drop(1), regex, updatedList)
                    } else {
                        findValidPasswordsToboggon(all.drop(1), regex, valid)
                    }
                }
                case _ => throw new IllegalArgumentException("Pattern not matched properly")
            }
        }

    }
}

