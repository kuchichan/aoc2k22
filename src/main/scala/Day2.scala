object Day2 {
  val draw = 3
  val win = 6

  val whoBeatsWho: Map[String, Tuple3[String, Int, String]] =
    Map(
      "X" -> ("C", 1, "A"), // Rock beats Scissors
      "Y" -> ("A", 2, "B"), // Paper beats Rock
      "Z" -> ("B", 3, "C") // Scissors beats Paper
    )
  val winLooseDraw = Map(
    "A" -> ("Y", "Z", "X"),
    "B" -> ("Z", "X", "Y"),
    "C" -> ("X", "Y", "Z")
  )

  val parsedInput: List[List[String]] =
    InputReader.loadInput("day_2.txt").map(s => s.split(" ").toList)

  def calculatePoint(clash_pair: List[String]): Int = clash_pair match
    case List(p1, p2) if p1 == whoBeatsWho(p2)._3 => whoBeatsWho(p2)._2 + draw
    case List(p1, p2) => {
      val (piece, point, _) = whoBeatsWho(p2)
      if piece == p1 then point + win else point
    }
    case invalid: List[String] => 0

  def calculateStrategy(clash_pair: List[String]): Int = clash_pair match
    case piece :: "Z" :: Nil   => whoBeatsWho(winLooseDraw(piece)._1)._2 + win
    case piece :: "Y" :: Nil   => whoBeatsWho(winLooseDraw(piece)._3)._2 + draw
    case piece :: "X" :: Nil   => whoBeatsWho(winLooseDraw(piece)._2)._2
    case invalid: List[String] => 0

  val answer_1 = parsedInput.foldLeft(0)((x, y) => x + calculatePoint(y))
  val answer_2 = parsedInput.foldLeft(0)((x, y) => x + calculateStrategy(y))
}
