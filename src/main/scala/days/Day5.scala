object Day5 extends Day {
  val day = 5

  def parseStackItem(item: String): Option[Char] = {
    if (item(1) == ' ') None else Some(item(1))
  }

  def buildStacks(stackString: String): List[List[Char]] = {
    val colCount =
      (stackString.indexOf("\n") / 4) + 1

    val stackItems = stackString.grouped(4).map(parseStackItem).toIndexedSeq

    println(s"cols: $colCount")
    println(s"items: ${stackItems.size}")

    val stacks = (0 until colCount)
      .map(col =>
        (0 until stackItems.size).filter(_ % colCount == col).map(stackItems(_)).toList
      )
      .toList

    stacks.map(_.flatMap(_.filter(_.isLetter).toList))
  }

  def main(input: String): Unit = {
    val parts = input.split("\n\n")
    val initialStacksString: String = parts(0)
    val instructionsStrings: List[String] = parts(1).split("\n").toList

    val stacks = buildStacks(initialStacksString)

    ???
  }
}
