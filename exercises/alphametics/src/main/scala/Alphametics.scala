import scala.collection.SortedSet

object Alphametics {

  type Answer = Map[Char, Int]

  case class CombinationsGenerator(size: Int) {
    val iterator: Iterator[List[Int]] = combinations(size, (0 to 9).toList)

    private def combinations(k: Int, list: List[Int]): Iterator[List[Int]] =
      list.combinations(k).flatMap(_.permutations)
  }

  case class AnswerGenerator(input: String) {
    def iterator: Iterator[Answer] = valuesGenerator.iterator.map(i => genMap(chars, i))

    private val chars: SortedSet[Char] = input.toUpperCase().foldLeft(SortedSet.empty[Char]) {
      case (acc, c) if c <= 'Z' && c >= 'A' => acc + c
      case (acc, _) => acc
    }

    private val valuesGenerator: CombinationsGenerator = CombinationsGenerator(chars.size)

    private def genMap(s: SortedSet[Char], values: List[Int]): Answer = s zip values toMap
  }

  sealed trait Value

  case class Expression(string: String) extends Value {
    def value(implicit translator: Answer): Long = {
      chunks.foldLeft(0L) {
        case (stringValue, str) => stringValue + str.map(translator).mkString.toLong
      }
    }

    private val chunks: Seq[String] = string.filterNot(_ == ' ').split('+').toList

    def isValid(implicit translator: Answer): Boolean = chunks.forall(s => translator(s.head) != 0)
  }

  case class Equation(left: Expression, right: Expression) {
    def isValid(implicit translator: Answer): Boolean =
      left.isValid &&
        right.isValid &&
        left.value == right.value
  }

  def solve(str: String): Option[Answer] = {
    val answerGenerator = AnswerGenerator(str)

    val equation: Option[Equation] = str.split(" == ").toList match {
      case l :: r :: Nil => Some(Equation(Expression(l), Expression(r)))
      case _ => None
    }

    for {
      eq <- equation
      answer <- answerGenerator.iterator.collectFirst {
        case answer if eq.isValid(answer) => answer
      }
    } yield answer
  }
}

