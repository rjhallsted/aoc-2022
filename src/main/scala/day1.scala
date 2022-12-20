import scala.io.Source
import java.net.URL

object Day1 {
    def main(): Unit = {
        val input = Source.fromURL(new URL("https://adventofcode.com/2022/day/1/input")).mkString
        
        val res = input.split("\n\n").map(_.split("\n").map(_.toInt).sum).max
        println(res)
    }
}