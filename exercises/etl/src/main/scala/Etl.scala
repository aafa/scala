object Etl {

  type OldFormat = Map[Int, Seq[String]]
  type NewFormat = Map[String, Int]

  def transform(input: OldFormat): NewFormat = {

    def accumulateAll(accumulated: NewFormat,
                      points: Int,
                      letters: Seq[String]): NewFormat = {
      letters.foldLeft(accumulated) {
        case (acc, letter) => acc.updated(letter.toLowerCase(), points)
      }
    }

    input.foldLeft(Map.empty[String, Int]) {
      case (acc, (points, letters)) => accumulateAll(acc, points, letters)
    }
  }
}
