object Day5 {
  val numbers = "\\d+".r
  val parsedCrates = InputReader
    .loadInput("day_5_crates.txt")
    .transpose
    .tail
    .zipWithIndex
    .filter((_, i) => i % 4 == 0)
    .map((a, i) => ((i / 4) + 1, a.filter(elem => elem.isUpper)))
    .toMap

  val parsedMoves = InputReader
    .loadInput(("day_5_moves.txt"))
    .map(x => numbers.findAllIn(x).toList)
    .map(_.map((_.toInt)))

  def move(
      crate: Map[Int, List[Char]],
      number: Int,
      from: Int,
      to: Int
  ): Map[Int, List[Char]] =
    (number, crate(from)) match
      case (_, Nil) => crate
      case (0, _)   => crate
      case (n, x :: xs) =>
        val c = crate.updated(from, xs)
        val c1 = c.updated(to, x :: c(to))
        move(c1, n - 1, from, to)

  def move9001(
      crate: Map[Int, List[Char]],
      number: Int,
      from: Int,
      to: Int
  ): Map[Int, List[Char]] =
    (number, crate(from)) match
      case (_, Nil) => crate
      case (0, _)   => crate
      case (n, xs) =>
        val c = crate.updated(from, xs.drop(n))
        c.updated(to, xs.take(n) ++ c(to))

  val answer_1 = parsedMoves
    .foldLeft(parsedCrates)((x, y) => move(x, y(0), y(1), y(2)))
    .toList
    .sortBy(x => x._1)
    .map(x => x._2.head)
    .mkString

  val answer_2 = parsedMoves
    .foldLeft(parsedCrates)((x, y) => move9001(x, y(0), y(1), y(2)))
    .toList
    .sortBy(x => x._1)
    .map(x => x._2.head)
    .mkString

}
