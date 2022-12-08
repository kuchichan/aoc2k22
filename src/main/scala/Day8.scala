import javax.swing.InputMap
object Day8 {
  val rows =
    InputReader.loadInput("day_8.txt").map(x => x.map(_.asDigit).toList)
  val columns = rows.transpose

  val drawMap = rows.zipWithIndex
    .map((line, row) =>
      line.zipWithIndex.map((elem, col) => ((row, col), elem))
    )
    .flatten
    .toMap

  val visibleBecauseItsEdge = 2 * rows.length + 2 * columns.length - 4

  def visibleInnerRow(tree_line: List[(Int, Int)], row: Int) =
    tree_line
      .dropRight(1)
      .tail
      .foldLeft((List[(Int, Int)](), tree_line.head._1))((acc, next) =>
        if (next._1 > acc._2) then ((row, next._2) :: acc._1, next._1) else acc
      )
      ._1

  def visibleInnerColumn(tree_line: List[(Int, Int)], column: Int) =
    tree_line
      .dropRight(1)
      .tail
      .foldLeft((List[(Int, Int)](), tree_line.head._1))((acc, next) =>
        if (next._1 > acc._2) then ((next._2, column) :: acc._1, next._1)
        else acc
      )
      ._1

  def calcScenicRight(
      mapPoint: ((Int, Int), Int),
      against: (Int, Int),
      acc: Int
  ): Int = against match
    case (a, b) if (b >= columns.length - 1) => acc
    case (a, b) =>
      if drawMap((a, b)) < mapPoint._2 then
        calcScenicRight(mapPoint, (a, b + 1), acc + 1)
      else acc

  def calcScenicLeft(
      mapPoint: ((Int, Int), Int),
      against: (Int, Int),
      acc: Int
  ): Int = against match
    case (a, b) if b <= 0 => acc
    case (a, b) =>
      if drawMap((a, b)) < mapPoint._2 then
        calcScenicLeft(mapPoint, (a, b - 1), acc + 1)
      else acc

  def calcScenicBottom(
      mapPoint: ((Int, Int), Int),
      against: (Int, Int),
      acc: Int
  ): Int = against match
    case (a, b) if a >= rows.length - 1 => acc
    case (a, b) =>
      if drawMap((a, b)) < mapPoint._2 then
        calcScenicBottom(mapPoint, (a + 1, b), acc + 1)
      else acc

  def calcScenicTop(
      mapPoint: ((Int, Int), Int),
      against: (Int, Int),
      acc: Int
  ): Int = against match
    case (a, b) if a <= 0 => acc
    case (a, b) =>
      if drawMap((a, b)) < mapPoint._2 then
        calcScenicTop(mapPoint, (a - 1, b), acc + 1)
      else acc

  def isEdge(mapPoint: ((Int, Int), Int), width: Int, length: Int): Boolean =
    mapPoint match
      case ((0, _), _)        => true
      case ((`width`, _), _)  => true
      case ((_, 0), _)        => true
      case ((_, `length`), _) => true
      case _                  => false

  def calcScenic(mapPoint: ((Int, Int), Int)): Int =
    calcScenicTop(mapPoint, (mapPoint._1._1 - 1, mapPoint._1._2), 1)
      * calcScenicBottom(mapPoint, (mapPoint._1._1 + 1, mapPoint._1._2), 1)
      * calcScenicRight(mapPoint, (mapPoint._1._1, mapPoint._1._2 + 1), 1)
      * calcScenicLeft(mapPoint, (mapPoint._1._1, mapPoint._1._2 - 1), 1)

  val visibleFromLeft = rows.zipWithIndex
    .drop(1)
    .dropRight(1)
    .map((line, index) => visibleInnerRow(line.zipWithIndex, index))
    .flatten

  val visibleFromRight = rows.zipWithIndex
    .drop(1)
    .dropRight(1)
    .map((line, index) => visibleInnerRow(line.zipWithIndex.reverse, index))
    .flatten

  val visibleFromTop = columns.zipWithIndex
    .drop(1)
    .dropRight(1)
    .map((line, index) => visibleInnerColumn(line.zipWithIndex, index))
    .flatten

  val visibleFromBottom = columns.zipWithIndex
    .drop(1)
    .dropRight(1)
    .map((line, index) => visibleInnerColumn(line.zipWithIndex.reverse, index))
    .flatten

  val result =
    (visibleFromLeft ++ visibleFromRight ++ visibleFromTop ++ visibleFromBottom).toSet.size + visibleBecauseItsEdge

  val result_2 = drawMap
    .filterNot(x => isEdge(x, rows.length - 1, columns.length - 1))
    .map((mapPoint) => calcScenic(mapPoint))
    .max
}
