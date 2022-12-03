object Day3 {
  val alphabet = ('a' to 'z') ++ ('A' to 'Z')
  val alphabetToIndex = alphabet.zip(1 to alphabet.length).toMap
  val parsedInput =
    InputReader.loadInput("day_3.txt").map(x => x.splitAt(x.length / 2))

  val parsedInputForTask2 = 
      InputReader.loadInput("day_3.txt").grouped(3)

  val commonItemInThrees = parsedInputForTask2.map(x => x.map(_.toSet).reduce(_.intersect(_))).flatMap(x => x.toList)

  val wrongPackedItems = parsedInput
    .map((x, y) => x.toSet.intersect(y.toSet))
    .flatMap((x) => x.toList)
   
   val punctation = wrongPackedItems.map((x) => alphabetToIndex(x))
   val answer_1 = punctation.sum
   val answer_2 = commonItemInThrees.map((x) => alphabetToIndex(x)).sum 

}
