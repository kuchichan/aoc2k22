import scala.annotation.newMain
object Day9 {
  type Pos = (Int, Int)
  type Command = (String, Int)

  val directions = Map(
    "R" -> (0, 1),
    "L" -> (0, -1),
    "U" -> (1, 0),
    "D" -> (-1, 0)
  )
  val parsedInput = InputReader
    .loadInput("day_9.txt")
    .map(_.split(" ").toList)
    .map(x => x match { case List(x, y) => (x, y.toInt) })

  def getDistance(headPos: Pos, tailPos: Pos): Pos =
    ((headPos._1 - tailPos._1).abs, (headPos._2 - tailPos._2).abs)

  def getDirection(headPos: Pos, tailPos: Pos): Pos =
    ((headPos._1 - tailPos._1).signum, (headPos._2 - tailPos._2).signum)

  def shouldTailMove(headPos: Pos, tailPos: Pos): Boolean =
    val (x, y) = getDistance(headPos, tailPos)
    x >= 2 || y >= 2 

  def add_pos(x: Pos, y: Pos): Pos =
      (x._1 + y._1, x._2 + y._2)

  def reorganizeRopeShape(rope: List[Pos]): List[Pos] =
      val newRope = rope.head :: rope.sliding(2).map((x => if shouldTailMove(x(0),x(1)) then 
          add_pos(x(1), getDirection(x(0), x(1))) else x(1))).toList
      if newRope == rope then rope else reorganizeRopeShape(newRope)

  def moveRope(direction: Pos, rope: List[Pos], acc: Int, visited: List[Pos]): (List[Pos], List[Pos]) = 
    if acc == 0 then (rope, visited) else 
      val newRope = reorganizeRopeShape(add_pos(rope.head, direction) :: rope.tail)
      moveRope(direction, newRope, acc - 1, newRope.last :: visited)

  def simulate(command: Command, rope: List[Pos], visited: List[Pos]) =
      val (letter, num) = command
      moveRope(directions(letter), rope, num, visited)

  val answer_1 = parsedInput.foldLeft((List.fill(2)((0,0)), List((0,0))))((acc , command) => simulate(command, acc._1, acc._2))._2.toSet.size
  val answer_2 = parsedInput.foldLeft((List.fill(10)((0,0)), List((0,0))))((acc , command) => simulate(command, acc._1, acc._2))._2.toSet.size

}
