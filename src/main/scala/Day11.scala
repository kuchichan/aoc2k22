object Day11 {
  case class Monke(items: List[Long], operation: Long => Long, test: Long => Int)
  def makeMonke(monkeInput: List[Any]): Monke = monkeInput match
    case List(
          items: List[Long],
          operation: List[String],
          divisble: Int,
          ifTrue: Int,
          ifFalse: Int, 
        ) =>
      Monke(
        items.map(x => x.toLong),
        parseOperation(operation),
        parseTest(divisble, ifTrue, ifFalse)
      )
    case _ => Monke(List(), x => x, x => x.toInt)

  val parsedInput = makeSplits(InputReader.loadInput("day_11.txt"), List())
    .map(x => cleanInput(x))
    .map(x => makeMonke(x))
    .zipWithIndex
    .map((x, y) => (y, (x, 0)))
    .toMap

  def makeSplits(
      input: List[String],
      acc: List[List[String]]
  ): List[List[String]] = input match
    case List()           => acc.reverse
    case xs: List[String] => makeSplits(input.drop(7), input.take(6) :: acc)

  def cleanInput(monke: List[String]): List[Any] =
    monke
      .drop(1)
      .zipWithIndex
      .map((x, index) =>
        index match
          case 0 =>
            x.split(":")
              .last
              .split(",")
              .map(x => x.filterNot(x => x.isWhitespace).toLong)
              .toList
          case 1 => x.split(" ").reverse.take(2).reverse.toList
          case _ => x.split(" ").last.toInt
      )

  def parseOperation(operation: List[String]): Long => Long = operation match
    case List("*", "old") => x => x * x
    case List("+", "old") => x => x + x
    case List("+", num)   => { val parsed = num.toLong; x => parsed + x }
    case List("*", num)   => { val parsed = num.toLong; x => parsed * x }

  def parseTest(divisble: Int, ifTrue: Int, ifFalse: Int): Long => Int =
    x => if x % divisble == 0 then ifTrue else ifFalse

  def monkePlay(monkeIndex: Int, monkeMap: Map[Int, (Monke, Int)]): Map[Int, (Monke, Int)] =
    val (monke, inc) = monkeMap(monkeIndex)
    monke.items match
      case List() => monkeMap
      case x :: xs =>
        val newWorryLevel = monke.operation(x) % (13 * 7 * 19 * 2 * 5 * 3 * 11 * 17).toLong  // lcm from all primeNumbers
        val toMonke = monke.test(monke.operation(x))

        val (nextMonke, nextInc) = monkeMap(toMonke)
        monkePlay(
          monkeIndex,
          monkeMap
            .updated(monkeIndex, (monke.copy(items = xs), inc + 1))
            .updated(
              toMonke,
              (nextMonke.copy(items = nextMonke.items.appended(newWorryLevel)), nextInc)
            )
        )

  val answer_1 = Stream
    .continually(0 to 3)
    .flatten
    .take(4 * 20)
    .foldLeft(parsedInput)((acc, x) => monkePlay(x, acc))
    .toList
    .map((a, b) => b._2)
    .sorted
    .reverse
    .take(2)
    .reduce((x, y) => x * y)

  val answer_2: Long = Stream
    .continually(0 to 7)
    .flatten
    .take(8 * 10000)
    .foldLeft(parsedInput)((acc, x) => monkePlay(x, acc))
    .toList
    .map((a, b) => b._2)
    .sorted.reverse.take(2)
    .foldRight(1.toLong)((x, y) => x.toLong * y.toLong)

}
