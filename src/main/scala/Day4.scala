object Day4 {
  val parsedInput = InputReader
    .loadInput("day_4.txt")
    .map(_.split(",").toList)
    .map(x => x.map(_.split("-").toList))

  val parsedToInt = parsedInput.map(x => x.map(x => x.map(_.toInt)))

  def isRangeFullyContained(x: List[Int], y: List[Int]): Boolean =
    x.head >= y.head && x.last <= y.last

  def isRangeOverlapped(x: List[Int], y: List[Int]): Boolean =
      y.head <= x.last

  val answer_1 = parsedToInt
    .filter(x =>
      isRangeFullyContained(x.head, x.last)
        || isRangeFullyContained(x.last, x.head)
    )
    .length

  val answer_2 = parsedToInt
    .filter(x =>
      isRangeOverlapped(x.head, x.last) 
      && isRangeOverlapped(x.last, x.head)
    )
    .length



}
