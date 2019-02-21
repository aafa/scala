object FlattenArray {
  def flatten(list: List[Any]): List[Any] = list match {
    case (head: List[_]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail if head != null => head :: flatten(tail)
    case _ => Nil
  }
}
