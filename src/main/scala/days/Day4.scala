object Day4 extends Day {
  val day = 4

  def rangesOverlap(r1: Range.Inclusive, r2: Range.Inclusive): Boolean = {
    (r1.end >= r2.start && r1.end <= r2.end) || // edge
    (r2.end >= r1.start && r2.end <= r1.end) || // edge
    (r1.start <= r2.start && r1.end >= r2.end) || // encompass
    (r2.start <= r1.start && r2.end >= r1.end) // encompass
  }

  def main(input: String): Unit = {
    val pairs = input
      .split("\n")
      .map(
        _.split(",")
          .map(_.split("-").map(_.toInt))
          .map(x => Range.inclusive(x(0), x(1)))
      )
      .map(x => (x(0), x(1)))
      .toList

    val res = pairs.count(p => rangesOverlap(p._1, p._2))

    println(res)
  }
}
