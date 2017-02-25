/**
 * Seven Languages in Seven Weeks
 * Scala, Day3
 *
 * Actors are not a part of the Scala standard library. I used
 * Akka to reimplement the example from the book and solved the
 * exercises based on that reimplementation.
 */

import scala.io._
import akka.actor._
import akka.actor.Actor
import Actor._

object PageLoader {
    def getPageSize(url: String) = Source.fromURL(url).mkString.length
    def getNumberOfLinks(url: String) = {
        val html = Source.fromURL(url).mkString
        val linkRegex = """<link.*?>""".r
        linkRegex.findAllIn(html).length
    }
}

class PageSizeActor extends Actor {
    def receive = {
        case url: String => {
            println("Size for " + url + ": " + PageLoader.getPageSize(url))
            println("Number of links for " + url + ": " + PageLoader.getNumberOfLinks(url))
        }
    }
}

object Day {

    val urls = List("http://www.archlinux.org/",
            "http://www.scala-lang.org/",
            "http://www.spiegel.de/",
            "http://www.cnn.com/")

    def timeMethod(method: () => Unit) = {
        val start = System.nanoTime
        method()
        val end = System.nanoTime
        println("Method took " + (end - start)/1000000000.0 + " seconds.")
    }

    def getPageSizeSequentially() = {
        for (url <- urls) {
            println("Size for " + url + ": " +
                    PageLoader.getPageSize(url))
            println("Number of links for " + url + ": " +
                    PageLoader.getNumberOfLinks(url))
       }
    }

    def getPageSizeConcurrently() = {
        val system = ActorSystem("Day")
        val pageSizeActor = system.actorOf(Props[PageSizeActor], name = "pageSizeActor")

        for (url <- urls) {
            pageSizeActor ! url
        }
    }

    def main(args: Array[String]): Unit = {
        println("Sequential run:")
        timeMethod { getPageSizeSequentially }
        println("Concurrent run:")
        timeMethod { getPageSizeConcurrently }
    }
}