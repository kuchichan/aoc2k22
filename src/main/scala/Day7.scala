object Day7 {
  case class FsNode(fsCount: Map[String, Int], curr_path: List[String])
  val parsedInput = InputReader.loadInput("day_7.txt")

  def dispatchCommand(command: String, fs: FsNode): FsNode =
    command.split(" ").toList match
      case List("$", "cd", "/")  => fs.copy(curr_path = List("/"))
      case List("$", "cd", "..") => fs.copy(curr_path = fs.curr_path.tail)
      case List("$", "cd", name) =>
        fs.copy(curr_path = s"${name}/${fs.curr_path.head}" :: fs.curr_path)
      case _ => fs

  def dispatchCommandResult(result: String, fs: FsNode): FsNode =
    result.split(" ").toList match
      case List("dir", name) => fs
      case List(number, name) => {
        val numberToInt = number.toInt
        val updatedSize = fs.curr_path.foldLeft(fs.fsCount)((acc, elem) =>
          acc.updated(elem, acc.getOrElse(elem, 0) + numberToInt)
        )
        fs.copy(fsCount = updatedSize)
      }
      case _ => fs

  def linesToNodes(lines: List[String], fs: FsNode): FsNode = lines match
    case Nil => fs
    case x :: xs => {
      if x.startsWith("$") then linesToNodes(xs, dispatchCommand(x, fs))
      else linesToNodes(xs, dispatchCommandResult(x, fs))
    }

  val mapfs = linesToNodes(parsedInput, FsNode(Map(), List()))

  val answer_1 =
    mapfs.fsCount.filter((_, size) => size <= 100000).map((x, y) => y).sum
  val answer_2 = mapfs.fsCount
    .filter((_, size) => 70000000 - mapfs.fsCount("/") + size >= 30000000)
    .map((x, y) => y)
    .min
}
