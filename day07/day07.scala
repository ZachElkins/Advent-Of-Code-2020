import scala.io.Source

object Day07 {
    def main(args: Array[String]): Unit = {
        
        println("Hang in there, this one might take a while to run...")

        // MARK: General Setup
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList

        // MARK: Part 1
        val bagNames: List[List[String]] = lst.map(_.split("( bags contain)?(,?\\s\\d\\s)").toList)
        val bagNamesClean: List[List[String]] = bagNames.map(data => {
            data.map(d => {
                if (d.endsWith("contain no other bags.")) d.substring(0, d.size-28)
                else if (d.endsWith("bags.")) d.substring(0, d.size-6)
                else if (d.endsWith("bags")) d.substring(0, d.size-5)
                else if (d.endsWith("bag.")) d.substring(0, d.size-5)
                else if (d.endsWith("bag")) d.substring(0, d.size-4)
                else d
            })
        })
        val bagContents: Map[String, List[String]] = bagNamesClean.map(data => (data.head -> data.tail)).toMap

        var count = 0
        for (bag <- bagContents.keys) {
            if (containsShinyGoldBag(bagContents, bag)) {
                count = count + 1
            }
        }
        println(s"There are $count way to bring a shiny gold bag!")

        // MARK: Part 2
        val bagNamesAndNumbers: List[List[String]] = lst.map(_.split("( bag)s?[.,]?").toList)
        val bagNamesAndNumbersClean: List[List[String]] = bagNamesAndNumbers.map(data => {
            data.map(d => {
                if (d.startsWith(" contain no other")) ""
                else if (d.startsWith(" contain ")) d.substring(9, d.size)
                else if (d.startsWith(" ")) d.substring(1, d.size)
                else d
            })
        })
        val bagContentsAndNumbers: Map[String, List[(Int, String)]] = bagNamesAndNumbersClean.map(data =>
            (data.head -> data.tail.map(d => {
                if (d.size >= 1) {
                    (d(0).toString.toInt -> d.substring(2, d.size))
                } else {
                    (-1, "")
                }
            }).toList.filter(_ != (-1, "")))).toMap

        val result = cotainsShinyGoldBagWithCounter(bagContentsAndNumbers, "shiny gold")

        println(s"You can fit $result bags in a single shiny gold bag")
    }

    def cotainsShinyGoldBagWithCounter(bagContents: Map[String, List[(Int, String)]], currentBag: String): Int = {
        val contents = bagContents(currentBag)
        if (contents == List.empty) {
            0
        } else {
            var count = 0
            for (b <- contents) {
                count = count + b._1 + b._1 * cotainsShinyGoldBagWithCounter(bagContents, b._2)
            }
            return count
        }
    }

    def containsShinyGoldBag(bagContents: Map[String, List[String]], currentBag: String): Boolean = {
        val contents = bagContents(currentBag)

        if (bagContents(currentBag).contains("shiny gold")) {
            // Base Case: found in bag
            true
        } else if (contents == List.empty){
            // Base Case: End of chain
            false
        } else {
            // Continue searching
            for (b <- contents) {
                containsShinyGoldBag(bagContents, b)
                if (containsShinyGoldBag(bagContents, b)) {
                    // println(s"Found another! $b inside a $currentBag")
                    return true
                }
                else false
            }
            false
        }
    }
}