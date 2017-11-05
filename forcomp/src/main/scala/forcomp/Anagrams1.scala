package forcomp

object Anagrams1 {
  
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]
  
  type Occurrences = List[(Char, Int)]
  
  val dictionary: List[Word] = loadDictionary
  
  def wordOccurrences(w: Word): Occurrences = {
    val chars = w.toLowerCase().toList
    (chars groupBy (c => c)).toList.map{case (x,y) => (x, y.length)}.sortBy(x => x._1)
  }

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatten.toString)
  
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences
  
  lazy val dictionary1 = dictionaryByOccurrences withDefaultValue Nil
  
  def wordAnagrams(word: Word): List[Word] = dictionary1(wordOccurrences(word))
  
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinationsAcc (occurrencesLeft: Occurrences, acc: List[Occurrences]): List[Occurrences] = occurrencesLeft match {
      case Nil => acc
      case (c,n)::xs => (for { m <- 1 to n
                                  occ <- acc} yield (c,m)::occ).toList 
    
    }
    combinationsAcc(occurrences, List(Nil))
  }



}