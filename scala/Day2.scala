/**
 * Seven Languages in Seven Weeks
 * Scala, day 2
 */

// 1.
// Compute total size of a list of strings using foldLeft
// I assume the length of all strings contained in the list should be summed
val strings = List("a", "bc", "def")
val totalLength = strings.foldLeft(0)((totalLength, s) => totalLength + s.length)

println(totalLength)


// 2.
// Write a Censor trait
trait Censor {
    val censoredWords = Map(
        "Shoot" -> "Pucky",
        "Darn" -> "Beans"
    )

    override def toString = {
        var uncensored = super.toString
        censoredWords.foldLeft(uncensored)((result, word) => result.replace(word._1, word._2))
    }
 }

// String is a final class and therefore it's not possible to extend with
// Because of this let's define a class 'Text'
class Text(content: String) {
    override def toString = {
        content
    }
}

val text = new Text("Shoot, Darn, Hey!") with Censor
println(text)


// 3.
// Load the curse words and alternatives from a file
import scala.io.Source

trait CensorFromFile extends Censor {

    override val censoredWords = initCensoredWords()

    def initCensoredWords() = {
        // It would be nice to pass the file from outside...
        val file = "curse_words.csv"
        val lines = Source.fromFile(file).getLines
        lines.map(_.split(";")).map(x => (x(0), x(1))).toMap
    }
}

val newText = new Text("Darn, Shoot, Hey!") with CensorFromFile
println(newText)