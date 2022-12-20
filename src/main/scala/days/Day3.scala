

object Day3 extends Day {
    val day = 3

    val lowercase = "abcdefghijklmnopqrstuvwxyz"
    val uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    def commonItems(group: List[String]): Set[Char] = {
        val fullSet = (lowercase + uppercase).toCharArray.toSet
        group.map(_.toCharArray.toSet).foldLeft(fullSet)((acc, s) => acc.intersect(s))
    }

    def main(input: String): Unit = {
        val sacks = input.split("\n").toList
        val groups = sacks.grouped(3).toList
        val badges = groups.map(commonItems(_).toList.head)

        val prioritySum = badges.map(c => {
            if (lowercase.contains(c)) lowercase.indexOf(c) + 1
            else uppercase.indexOf(c) + 27
        }).sum

        println(prioritySum)
    }
}