object Day10 extends Day {
  override def day: Int = 10

  final case class Cycle(number: Int, startingX: Int, endingX: Int)
  final case class CycleAdj(v: Int)

  def cycleAdjsFromLine(line: String): List[CycleAdj] = {
    val adj = line.split(" ")(1).toInt
    List(CycleAdj(0), CycleAdj(adj))
  }

  def cyclesFromInstructions(input: String): List[CycleAdj] = {
    val lines = input.split("\n").toList
    lines.flatMap(_ match {
      case line if line.startsWith("addx") => cycleAdjsFromLine(line)
      case _                               => List(CycleAdj(0))
    })
  }

  def adjustCycle(cycles: List[Cycle], cycleAdj: CycleAdj): List[Cycle] = {
    val lastCycle = cycles.head
    val newCycle = Cycle(
      lastCycle.number + 1,
      lastCycle.endingX,
      lastCycle.endingX + cycleAdj.v
    )
    newCycle :: cycles
  }

  def signalStrength(cycle: Cycle): Int = cycle.number * cycle.startingX

  def pixelValue(cycle: Cycle): String =
    if (spriteVisible(cycle)) "#" else "."

  def spriteVisible(cycle: Cycle): Boolean = {
    val spritePos = cycle.startingX
    val drawPos = (cycle.number - 1) % 40
    (drawPos >= spritePos - 1 && drawPos <= spritePos + 1)
  }

  def drawImage(pixels: List[String]): Unit = {
    val width = 40
    println()
    pixels.grouped(width).map(_.mkString).foreach(println)
    println()
  }

  override def main(input: String): Unit = {
    val cycleAdjs = cyclesFromInstructions(input)

    val startingCycle = Cycle(0, 1, 1)
    val cycles = cycleAdjs.foldLeft(startingCycle :: Nil)(adjustCycle).reverse

    // part1
    // val interestingCycles = Set(20, 60, 100, 140, 180, 220)
    // val sum = cycles.filter(interestingCycles contains _.number).map(signalStrength).sum
    // println(sum)

    // part2
    val pixels = cycles.drop(1).map(pixelValue)
    drawImage(pixels)
  }

}
