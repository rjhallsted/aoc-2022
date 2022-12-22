object Day6 extends Day {
  val day = 6

  val windowSize: Int = 14

  def uniqCharsOnly(in: String): Boolean = in.toSet.size == in.size

  def newWindow(window: String, c: Char): String = {
    if (window.length <= windowSize - 1) {
      window + c
    } else window.substring(1) + c
  }

  def main(input: String): Unit = {
    val firstPacketAt =
      input.toCharArray.zipWithIndex.foldLeft[(String, Option[Int])](
        ("", None)
      )((state: (String, Option[Int]), pair: (Char, Int)) => {
        val (window, packetAt) = state
        val (c, idx) = pair

        val nw = newWindow(window, c)

        val pa = packetAt.orElse({
          if (nw.size == windowSize && uniqCharsOnly(nw)) Some(idx) else None
        })

        (nw, pa)
      })._2

    println(firstPacketAt.map(_ + 1))
  }
}
