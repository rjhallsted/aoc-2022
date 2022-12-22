import zio._
import zio.http._
import scala.io.Source

object Main extends ZIOAppDefault {
  val day = 8

  val program = for {
    input <- AOCClient.getInput(day)
    // input <- ZIO.succeed(Source.fromFile("src/main/scala/inputs/sample8.txt").mkString)
  } yield Day8.main(input)

  override val run = program.provideLayer(Client.default)
}