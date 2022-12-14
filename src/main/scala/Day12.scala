import scala.collection.immutable.Queue

object Day12 {
  type Pos = (Int, Int)

  val parsedInput = InputReader
    .loadInput("day_12.txt")
    .map(x => x.toCharArray().toList)

  def getElevation(x: Int, y: Int) = 
    parsedInput(x)(y) match
      case 'E' => 'z'.toInt
      case 'S' => 'a'.toInt
      case c => c.toInt 

  def possibleMoves(pos: Pos, visited: Set[Pos]): List[Pos] =
    val (a, b) = pos
    List((a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1))
      .filter((x, y) => (x >= 0 && x < parsedInput.size) && (y >= 0 && y < parsedInput(0).size))
      .filter(x => !visited.contains(x))
      .filter((x, y) => getElevation(x,y) - getElevation(a,b) <= 1)
      .toList

  def traverse(
      moves: Queue[(Pos, List[Pos])],
      visited: Set[Pos]
  ): (Pos, Set[Pos], List[Pos]) =
    if moves.isEmpty then ((0, 0), Set(), List())
    else
      val (((x, y), route), queue) = moves.dequeue
      if parsedInput(x)(y) == 'E' then ((x, y), visited, route)
      else
        val newMoves = possibleMoves((x, y), visited + ((x, y)))
        traverse(
          queue.distinctBy(a => a._1).enqueueAll((newMoves.map(a => (a, route.appended((x, y)))))),
          visited + ((x, y))
        )

  lazy val answer_1 = traverse(Queue(((20, 0), List())), Set((20, 0)))._3.size
  lazy val answer_2 = parsedInput.zipWithIndex
    .map((x, i1) => x.zipWithIndex.map((y, i2) => (y, (i1, i2))))
    .flatten
    .filter(x => (x._1 == 'a' || x._1 == 'S'))
    .map(x => traverse(Queue((x._2, List())), Set(x._2)))
    .map(x => x._3.size)
    .filter(x => x != 0)
    .min
}
