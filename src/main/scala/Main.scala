import zio._
import zio.http._
import scala.io.Source

object Main extends ZIOAppDefault {
  val dayProgram = Day10

  val program = for {
    input <- AOCClient.getInput(dayProgram.day)
    // input <- ZIO.succeed(Source.fromFile(s"src/main/scala/inputs/sample${dayProgram.day}.txt").mkString)
  } yield dayProgram.main(input)

  override val run = program.provideLayer(Client.default)
}