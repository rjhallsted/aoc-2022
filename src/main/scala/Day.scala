import zio._
import zio.http.Client

trait Day {
    type Res[U] = RIO[Client, U]

    def day: Int 

    def main(input: String): Unit
}