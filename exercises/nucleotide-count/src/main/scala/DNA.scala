class DNA(dna: String) {
  type Answer = Map[Char, Int]
  type Result = Either[String, Answer]

  private val nucleotides: Set[Char] = Set('A', 'T', 'G', 'C')
  private val EmptyResultE: Result = Right(
    Map('A' -> 0, 'T' -> 0, 'G' -> 0, 'C' -> 0))

  private def traverseString(chain: String): Result =
    chain.foldLeft(EmptyResultE) {
      case (result, char) =>
        for {
          nucleotide <- validNucleotide(char)
          answer <- result
        } yield answer.updated(nucleotide, answer.getOrElse(nucleotide, 0) + 1)
    }

  private def validNucleotide(char: Char): Either[String, Char] = {
    if (nucleotides.contains(char)) {
      Right(char)
    } else {
      Left("Wrong nucleotide char")
    }
  }

  def nucleotideCounts: Result = traverseString(dna)

}
