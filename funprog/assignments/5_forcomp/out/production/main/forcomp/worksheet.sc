  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]


  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase.toList.groupBy { case i => i }.map { x => (x._1, x._2.length) }.toList.sortWith(_._1 < _._1)
  }

  val sen = List("abc", "def")


  val emptyMap = Map[Char, Int]() withDefaultValue 0


    def addCharOccurrences(charOccurrences: Map[Char, Int], charCount: (Char, Int)): Map[Char, Int] = {
      val (char, count) = charCount
      charOccurrences + (char -> (charOccurrences(char) + count))
    }

    (sen.map(wordOccurrences(_).toMap.withDefaultValue(0))).foldLeft(emptyMap)(addCharOccurrences)


    val m1 = wordOccurrences("abc").toMap.withDefaultValue(0)
    val m2 = wordOccurrences("abcdef").toMap.withDefaultValue(0)

def sumMaps(map: Map[Char, Int], otherMap: Map[Char, Int]): Map[Char, Int] = {
  def sumCharCount(map: Map[Char, Int], charCount: (Char, Int)): Map[Char, Int] = {
    val (char, count) = charCount
    map + (char -> (map(char) + count))
  }

  (map.foldLeft(otherMap)(sumCharCount))
  // Map[Char, Int]() withDefaultValue 0
}


m1.foldLeft(m2)(x:(Char, Int) => println(x._1))
