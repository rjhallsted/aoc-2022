import zio._
import zio.http._

object Main extends ZIOAppDefault {
  val day = 6

  val program = for {
    input <- AOCClient.getInput(day)
  } yield Day6.main(input)

  override val run = program.provideLayer(Client.default)
}