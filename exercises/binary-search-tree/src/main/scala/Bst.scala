sealed trait Node[+T] {
  def value: T

  def get: Node[T] // to respect API calls from tests

  def left: Node[T]

  def right: Node[T]

  def insert[B >: T](that: B)(implicit ordering: Ordering[B]): Node[B]

  def toList: List[T]
}

case class BstNode[T](value: T, left: Node[T], right: Node[T]) extends Node[T] {

  def insert[B >: T](that: B)(implicit ordering: Ordering[B]): Node[B] = {
    append(that, this)
  }

  private def append[B >: T](that: B, tree: Node[B])(
    implicit ordering: Ordering[B]): Node[B] = tree match {
    case BstNode(v, l, r) if ordering.gt(that, v) =>
      BstNode(v, l, append(that, r))
    case BstNode(v, l, r) => BstNode(v, append(that, l), r)
    case NilNode => Bst(that)
  }

  override def toString: String = s"$value (left: $left; right: $right)"

  override def get: Node[T] = this

  override def toList: List[T] = this.left.toList ::: value :: this.right.toList
}

object Run extends App {
  print(Bst.fromList(List(1, 2, 3)))
}

case object NilNode extends Node[Nothing] {
  override def value: Nothing = throw new Exception("no value in the empty node")

  override def get: Node[Nothing] = NilNode

  override def left: Node[Nothing] = NilNode

  override def right: Node[Nothing] = NilNode

  override def insert[B >: Nothing : Ordering](that: B): Node[B] = NilNode

  override def toList: List[Nothing] = Nil
}

object Bst {
  def apply[T](value: T): Node[T] = BstNode[T](value, NilNode, NilNode)

  def toList[T](value: Node[T]): List[T] = value.toList

  def fromList[T](input: List[T])(implicit ordering: Ordering[T]): Node[T] = {
    input match {
      case Nil => NilNode
      case ::(head, tail) =>
        tail.foldLeft(apply(head)) {
          case (acc, value) => acc.insert(value)
        }
    }
  }

}
