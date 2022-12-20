import zio._
import zio.http._

object Main extends ZIOAppDefault {
  val day = 3

  val program = for {
    input <- AOCClient.getInput(day)
  } yield Day3.main(input)

  override val run = program.provideLayer(Client.default)
}