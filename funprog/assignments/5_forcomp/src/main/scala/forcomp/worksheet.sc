type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy { case i => i }.map { x => (x._1, x._2.length) }.toList.sortWith(_._1 < _._1)
}

def combinations(occurrences: Occurrences): List[Occurrences] = {
  for {
    (char, count) <- occurrences
    i <- (1 to count)
    yield (char, i)
  }
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


def combinations(occurrences: Occurrences): List[Occurrences] = {
  for {
    (char, count) <- occurrences
    i <- (1 to count)
    yield (char, i)
  }
}
