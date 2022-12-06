object Day6 {
  val parsedInput = InputReader.loadInput("day_6.txt")
  val startOfPacketSize = 4
  val startOfMessageSize = 14

  val answer_1 = parsedInput.head
    .sliding(startOfPacketSize)
    .indexWhere(x => x.toSet.size == x.size) + startOfPacketSize

  val answer_2 = parsedInput.head
    .sliding(startOfMessageSize)
    .indexWhere(x => x.toSet.size == x.size) + startOfMessageSize 
}
