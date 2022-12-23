object Day8 extends Day {
  val day = 8

  def matrixFromString(input: String): Array[Array[Int]] = {
    input.split("\n").map(_.toCharArray.map(_.toString.toInt))
  }

  def newViewDistanceMatrix(height: Int, width: Int): Array[Array[Int]] = {
    (0 until height).toArray.map(_ => (0 until width).map(_ => 1).toArray)
  }

  def calcViewDistance(
      trees: Array[Array[Int]],
      viewDistance: Array[Array[Int]]
  ): Unit = {
        val height = trees.size
        val width = trees(0).size

        def walkUp(r: Int, c: Int, viewingHeight: Int): Int = {
            for (vr <- r-1 to 0 by -1) {
                if (trees(vr)(c) >= viewingHeight)
                    return r - vr
            }

            return List(r, 0).max
        }

        def walkRight(r: Int, c: Int, viewingHeight: Int): Int = {
            for (vc <- c+1 until width) {
                if (trees(r)(vc) >= viewingHeight)
                    return vc -c
            }

            return List(width-1 - c, 0).max
        }

        def walkDown(r: Int, c: Int, viewingHeight: Int): Int = {
            for (vr <- r+1 until height) {
                if (trees(vr)(c) >= viewingHeight)
                    return vr - r
            }

            return List(height-1 - r, 0).max
        }

        def walkLeft(r: Int, c: Int, viewingHeight: Int): Int = {
            for (vc <- c-1 to 0 by -1) {
                if (trees(r)(vc) >= viewingHeight)
                    return c - vc
            }

            return List(c, 0).max
        }

        for (r <- 0 until height) {
            for (c <- 0 until width) {
                val thisTree = trees(r)(c)
                viewDistance(r)(c) *= walkUp(r, c, thisTree)
                viewDistance(r)(c) *= walkRight(r, c, thisTree)
                viewDistance(r)(c) *= walkDown(r, c, thisTree)
                viewDistance(r)(c) *= walkLeft(r, c, thisTree)
            }
        }
  }

  def main(input: String): Unit = {
    val trees = matrixFromString(input)
    val viewDistance = newViewDistanceMatrix(trees.size, trees.head.size)

    // trees.foreach(r => println(r.toList))
    // println("")

    calcViewDistance(trees, viewDistance)

    val maxVisibility = viewDistance.flatten.max

    // viewDistance.foreach(r => println(r.toList))

    println(maxVisibility)
  }
}
