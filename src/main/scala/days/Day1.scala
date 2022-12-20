import scala.io.Source
import java.net.URL

object Day1 extends Day {

  def day: Int = 1
  def main(input: String): Unit = {
    val res = input
      .split("\n\n")
      .map(_.split("\n").map(_.toInt).sum)
      .sorted
      .reverse
      .take(3)
      .sum
    println(res)
  }
}
