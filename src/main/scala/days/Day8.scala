object Day8 extends Day {
  val day = 8

  def matrixFromString(input: String): Array[Array[Int]] = {
    input.split("\n").map(_.toCharArray.map(_.toInt))
  }

  def newVisibilityMatrix(height: Int, width: Int): Array[Array[Boolean]] = {
    (0 until height).toArray.map(_ => (0 until width).map(_ => false).toArray)
  }

  def markVisibleTrees(
      trees: Array[Array[Int]],
      visible: Array[Array[Boolean]]
  ): Unit = {
    def fromLeft(): Unit = {
        visible.foreach(r => r(0) = true)
        
        for (r <- 0 until visible.size) {
            var height = trees(r)(0)
            for (c <- 1 until visible(0).size) {
                if (trees(r)(c) > height) {
                    height = trees(r)(c)
                    visible(r)(c) = true
                }
            }
        }
    }

    def fromTop(): Unit = {
        for (c <- 0 until visible(0).size) {
            visible(0)(c) = true
        }

        for (c <- 0 until visible(0).size) {
            var height = trees(0)(c)
            for (r <- 1 until visible.size) {
                if (trees(r)(c) > height) {
                    height = trees(r)(c)
                    visible(r)(c) = true
                }
            }
        }
    }

    def fromRight(): Unit = {
        visible.foreach(r => r(r.size - 1) = true)
        
        for (r <- 0 until visible.size) {
            var height = trees(r)(trees(r).size - 1)
            for (c <- visible(0).size-2 to 0 by -1) {
                if (trees(r)(c) > height) {
                    height = trees(r)(c)
                    visible(r)(c) = true
                }
            }
        }
    }

    def fromBottom(): Unit = {
        for (c <- 0 until visible(0).size) {
            visible(visible.size-1)(c) = true
        }

        for (c <- 0 until visible(0).size) {
            var height = trees(visible.size-1)(c)
            for (r <- visible.size-2 to 0 by -1) {
                if (trees(r)(c) > height) {
                    height = trees(r)(c)
                    visible(r)(c) = true
                }
            }
        }
    }

    fromLeft()
    fromBottom()
    fromRight()
    fromTop()
  }

  def main(input: String): Unit = {
    val trees = matrixFromString(input)
    val visible = newVisibilityMatrix(trees.size, trees.head.size)

    markVisibleTrees(trees, visible)
    val visibleCount = visible
      .map(_.map(_ match {
        case true  => 1
        case false => 0
      }).sum)
      .sum

    println(visibleCount)
  }
}
