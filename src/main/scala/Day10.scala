object Day10 {
  type Cycles = List[(Int, Int)]
  var parsedInput = InputReader
    .loadInput("day_10.txt")
    .map(_.split(" ").toList)
    .map(x =>
      x match {
        case List(x, y) => (x, y.toInt)
        case List(x)    => x
      }
    )

  val checkingWindow = List(20, 60, 100, 140, 180, 220)

  def calculateAmp(cycles: (Int, Int)): Int =
    cycles._1 * cycles._2

  def drawSprite(row: String, pos: Int): String =
    row.zipWithIndex
      .map((x, index) =>
        if (index >= pos - 1 && index <= pos + 1) then
          x match {
            case '#' => '.'
            case '.' => '#'
          }
        else x
      )
      .mkString

  def draw(row: String, pos: Int, lastPos: Int): (String, Int) =
    val newRow =
      if (row.size - 1 <= lastPos && row.size + 1 >= lastPos) then row + "#"
      else row + "."
    if pos != lastPos then (newRow, pos) else (newRow, pos)

  def runCommand(command: Object, cycles: Cycles): Cycles = command match
    case (command, value: Int) =>
      (cycles.head._1 + 2, cycles.head._2 + value) :: (
        cycles.head._1 + 1,
        cycles.head._2
      ) :: cycles
    case _ =>
      ((cycles.head._1 + 1, cycles.head._2) :: cycles)

  val cyclesList = parsedInput
    .foldLeft(List((0, 1)))((acc, command) => runCommand(command, acc))
    .reverse

  def makeScreenInput(cycles: Cycles, acc: List[Cycles]): List[Cycles] =
    if cycles.isEmpty then acc
    else makeScreenInput(cycles.drop(40), cycles.take(40).toList :: acc)

  val answer_1 = cyclesList.zipWithIndex
    .filter((_, index) => checkingWindow.contains(index + 1))
    .map((x, y) => (x._2, y + 1))
    .map(x => calculateAmp(x))
    .sum

  val screenInput = makeScreenInput(cyclesList.drop(1), List()).reverse

  val answer_2 = screenInput
    .map(x =>
      x.foldLeft(("", 1))((acc, cycle) => draw(acc._1, cycle._2, acc._2))
    )
    .map(x => x._1)
    .reduce((acc, x) => acc + "\n" + x)
}
