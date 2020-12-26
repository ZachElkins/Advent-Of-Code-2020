import scala.io.Source
import scala.util.matching.Regex

object Day07 {
    def main(args: Array[String]): Unit = {
        val filename = "input.txt"
        val lst = Source.fromFile(filename).getLines.toList
        val bagNames: List[List[String]] = lst.map(_.split(raw"( bags contain)?(,?\s\d\s)").toList)
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
        val bagTypes: List[String] = bagNamesClean.map(_.head)

        var count = 0
        for (bag <- bagContents.keys) {
            if (containsShinyGoldBag(bagContents, bag)) {
                count = count + 1
            }
        }
        println(s"There are $count way to bring a shiny gold bag!")
        
        // containsShinyGoldBag(bagContents, "striped lime")
        // containsShinyGoldBag(bagContents, "dull gold")
        // containsShinyGoldBag(bagContents, "bright orange")

    }

    def containsShinyGoldBag(bagContents: Map[String, List[String]], currentBag: String): Boolean = {
        // println(bagContents(currentBag))
        val contents = bagContents(currentBag)

        if (bagContents(currentBag).contains("shiny gold") ) {
            println(s"Found it! $currentBag")
            true
        } else if (contents == List.empty){
            // println("Dead end!")
            false
        } else {
            // println("keep Looking")
            for (b <- contents) {
                if (containsShinyGoldBag(bagContents, b)) {
                    println(s"Found another! $b inside a $currentBag")
                    return true
                }
                else false
            }
            false
        }
    }
}