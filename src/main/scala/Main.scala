import zio._
import zio.http._
import scala.io.Source

object Main extends ZIOAppDefault {
  val day = 7

  val program = for {
    input <- AOCClient.getInput(day)
    // input <- ZIO.succeed(Source.fromFile("src/main/scala/inputs/sample7.txt").mkString)
  } yield Day7.main(input)

  override val run = program.provideLayer(Client.default)
}