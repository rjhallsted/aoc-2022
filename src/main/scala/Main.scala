import zio._
import zio.http._

object Main extends ZIOAppDefault {
  val day = 4

  val program = for {
    input <- AOCClient.getInput(day)
  } yield Day4.main(input)

  override val run = program.provideLayer(Client.default)
}