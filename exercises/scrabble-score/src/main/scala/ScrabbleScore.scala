object ScrabbleScore {

  // etl for input data from readme ;) to save some typing time
  private val points: Map[Char, Int] = {
    Map("A, E, I, O, U, L, N, R, S, T" -> 1,
      "D, G" -> 2,
      "B, C, M, P" -> 3,
      "F, H, V, W, Y" -> 4,
      "K" -> 5,
      "J, X" -> 8,
      "Q, Z" -> 10
    ).foldLeft(Map.empty[Char, Int]) {
      case (charMap, (string, points)) => string.filterNot(_ == ',').filterNot(_ == ' ').foldLeft(charMap) {
        case (acc, char) => acc.updated(char, points)
      }
    }
  }

  def score(input: String): Int = {
    input.foldLeft(0) {
      case (result, char) => result + points.getOrElse(char.toUpper, 0)
    }
  }
}
