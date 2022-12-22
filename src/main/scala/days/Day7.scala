import scala.annotation.tailrec
object Day7 extends Day {
  sealed trait Node {
    def name: String
    def parent: Option[Directory]
  }
  class Directory(
      var name: String,
      var parent: Option[Directory],
      var contents: List[Node] = List[Node]()
  ) extends Node {
    var contentsSize = 0

    def addFile(l: String): Unit = {
      this.contents = newFile(l, this) :: this.contents
      this.contentsSize += 1
    }

    def addDir(l: String): Unit = {
      this.contents = newDir(l, this) :: this.contents
      this.contentsSize += 1
    }
  }
  class File(
      var name: String,
      var size: Long,
      var parent: Option[Directory]
  ) extends Node

  val day = 7

  val filesystemSize: Long = 70000000
  val neededSpace: Long = 30000000

  def isLS(l: String): Boolean = l == "$ ls"
  def isCD(l: String): Boolean = l.startsWith("$ cd")
  def isCDRoot(l: String): Boolean = l == "$ cd /"
  def isCDUp(l: String): Boolean = l == "$ cd .."

  @tailrec
  def goToRoot(node: Directory): Directory = {
    node.parent match {
      case None    => node
      case Some(p) => goToRoot(p)
    }
  }

  def getCDName(l: String): String = l.substring(5)
  def isFile(l: String): Boolean = (!isLS(l) && !isCD(l) && !isDir(l))
  def newFile(l: String, parent: Directory): File = {
    val filePattern = """(\d+) (.*)""".r
    val filePattern(size, name) = l
    new File(name, size.toLong, Some(parent))
  }
  def isDir(l: String): Boolean = l.startsWith("dir")
  def newDir(l: String, parent: Directory): Directory =
    new Directory(l.substring(4), Some(parent))

  def findDir(name: String, contents: List[Node]): Directory = {
    contents
      .collect[Directory]({ case x: Directory => x })
      .find(_.name == name)
      .get
  }

  def handleLineAndGetNextRoot(line: String, root: Directory): Directory = {
    line match {
      case l if isCDRoot(l) => goToRoot(root)
      case l if isCDUp(l)   => root.parent.get
      case l if isLS(l)     => root
      case l if isCD(l)     => findDir(getCDName(l), root.contents)
      case l if isFile(l) => {
        root.addFile(l)
        root
      }
      case l if isDir(l) => {
        root.addDir(l)
        root
      }
    }
  }

  @tailrec
  def buildDirTree(lines: List[String], root: Directory): Directory = {
    lines.headOption match {
      case None => root
      case Some(line) =>
        buildDirTree(lines.tail, handleLineAndGetNextRoot(line, root))
    }
  }

  def getSmallestDeletableDirSize(root: Directory, filesystemSize: Long): Long = {
    // returns (dirSize, sumOfUnderThreshold)
    def getDirectorySizes(dir: Directory, sizes: List[Long]): List[Long] = {
        val files = dir.contents.collect({ case f: File => f.size}).sum
        val directoryResults = dir.contents.collect({case d: Directory => getDirectorySizes(d, sizes)})
        val directorySizes = directoryResults.map(_.headOption.getOrElse(0L)).sum

        List(files + directorySizes) ++ directoryResults.flatten ++ sizes
    }

    val sizes = getDirectorySizes(root, List[Long]())
    val threshold = neededSpace - (filesystemSize - sizes.head)
    println(s"threshold: $threshold")
    println(sizes.filter(_ >= threshold))
    sizes.filter(_ >= threshold).min
  }

  def main(input: String): Unit = {
    val lines = input.split("\n").toList
    val tree = new Directory("/", None)
    buildDirTree(lines, tree)

    val res = getSmallestDeletableDirSize(tree, filesystemSize)
    println(res)
  }
}
