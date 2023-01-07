object Day9 extends Day {
  val day = 9

  def moveFromInstruction(ins: String): (Int, Int) =
    ins match {
      case "U" => (0, 1)
      case "R" => (1, 0)
      case "D" => (0, -1)
      case "L" => (-1, 0)
    }

  def getInstructions(input: String): List[(Int, Int)] = {
    val pattern = """([A-Z]) (\d+)""".r

    input
      .split("\n")
      .toList
      .flatMap(l => {
        val pattern(ins, amount) = l
        List.fill(amount.toInt)(moveFromInstruction(ins))
      })
  }

  def moveBy(pos: (Int, Int), move: (Int, Int)): (Int, Int) =
    (pos._1 + move._1, pos._2 + move._2)

  def touching(headPos: (Int, Int), tailPos: (Int, Int)): Boolean = {
    val diff = positionDiff(headPos, tailPos)
    Math.abs(diff._1) <= 1 && Math.abs(diff._2) <= 1
  }

  def positionDiff(p1: (Int, Int), p2: (Int, Int)): (Int, Int) =
    (p1._1 - p2._1, p1._2 - p2._2)

  // magnitude
  def mag(x: Int): Int = Math.abs(x)

  // adjust by 1 towards zero
  def clamp(x: Int): Int = if (x > 0) x - 1 else x + 1

  def adjustTail(newHeadPos: (Int, Int), tailPos: (Int, Int)): (Int, Int) = {
    if (!touching(newHeadPos, tailPos)) {
      val adjustment = positionDiff(newHeadPos, tailPos) match {
        case (x, y) if mag(x) == mag(y) => (clamp(x), clamp(y))
        case (x, y) if mag(x) > mag(y)  => (clamp(x), y)
        case (x, y) if mag(x) < mag(y)  => (x, clamp(y))
      }
      moveBy(tailPos, adjustment)
    } else tailPos
  }

  def handleInstruction(
      state: (Set[(Int, Int)], List[(Int, Int)]),
      instruction: (Int, Int)
  ): (Set[(Int, Int)], List[(Int, Int)]) = {
    val (visited, rope) = state

    val newHeadPos = moveBy(rope.head, instruction)
    val newRope = rope.tail.foldLeft(newHeadPos :: Nil)((rope, pos) => {
      val newTailPos = adjustTail(rope.head, pos)
      newTailPos :: rope
    })
    // head is tail since newRope is in reverse order
    val nowVisited = visited + newRope.head // p2 method
    // val nowVisited = visited + newRope.reverse.tail.head // part 1 method
    (nowVisited, newRope.reverse)
  }

  def main(input: String): Unit = {
    val instructions = getInstructions(input)

    val visitedSpaces = Set[(Int, Int)]((0, 0))
    val positions = List.fill(10)((0, 0))

    val (finalVisitedSpaces, _) = instructions.foldLeft(
      (visitedSpaces, positions)
    )(handleInstruction)

    println(finalVisitedSpaces.size)
  }
}
