object Day5 extends Day {
  val day = 5

  def parseStackItem(item: String): Option[Char] = {
    if (item(1) == ' ') None else Some(item(1))
  }

  def buildStacks(stackString: String): IndexedSeq[List[Char]] = {
    val colCount =
      (stackString.indexOf("\n") / 4) + 1

    val stackItems = stackString.grouped(4).map(parseStackItem).toIndexedSeq

    val stacks = (0 until colCount)
      .map(col =>
        (0 until stackItems.size).filter(_ % colCount == col).map(stackItems(_)).toList
      )
      .toIndexedSeq

    stacks.map(_.flatMap(_.filter(_.isLetter).toList))
  }

  final case class Instruction(
    count: Int,
    from: Int,
    to: Int
  )

  def executeInstruction(stack: IndexedSeq[List[Char]], ins: Instruction): IndexedSeq[List[Char]] = {
    val moving = stack(ins.from).take(ins.count)
    val removed = stack.updated(ins.from, stack(ins.from).drop(ins.count))
    val added = removed.updated(ins.to, moving ++ removed(ins.to))

    added
  }

  def main(input: String): Unit = {
    val parts = input.split("\n\n")
    val initialStacksString: String = parts(0)
    val instructionsStrings: List[String] = parts(1).split("\n").toList

    val stacks = buildStacks(initialStacksString)

    val instructionPattern = """move (\d+) from (\d+) to (\d+)""".r
    val instructions: List[Instruction] = instructionsStrings.map(s => {
        val instructionPattern(count, from, to) = s
        Instruction(count.toInt, from.toInt - 1, to.toInt - 1)
    })

    val finished = instructions.foldLeft(stacks)(executeInstruction)
    val res = finished.flatMap(_.take(1)).mkString

    println(res)
  }
}
