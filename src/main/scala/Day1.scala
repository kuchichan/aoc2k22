object Day1 {

    def multipleSpan(acc: List[List[String]], aocList: List[String]): List[List[String]] = 
     aocList match
          case List() => acc
          case "" :: xs => {
               val (a, b) = xs.span(p => p != "")
               multipleSpan(a :: acc, b)
          }  
          case xs => {
               val (a, b) = xs.span(p => p != "") 
               multipleSpan(a :: acc, b)
          }

    val common_part = multipleSpan(List(), InputReader.loadInput(("day_1.txt")))
        .map(list => list.map(s => s.toInt))
        .map(list => list.sum)

    val answer_1 = common_part.max

    val answer_2 = common_part 
        .sorted
        .reverse
        .take(3)
        .sum
}
