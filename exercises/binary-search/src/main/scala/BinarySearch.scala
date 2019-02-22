object BinarySearch {

  /*
    returns the position of the target int
    */
  def find(ints: List[Int], target: Int): Option[Int] = findUsingAnArray(ints, target)

  /*
      O(logN) in time
      O(N) in space (array allocation)
   */
  private def findUsingAnArray(ints: List[Int], target: Int): Option[Int] = {
    val array = ints.toArray
    var left: Int = 0
    var right: Int = array.length - 1

    while (right >= left) {
      val mid: Int = (left + right) / 2
      val current = array(mid)

      if (current == target) {
        return Some(mid)
      } else if (current < target) { // go right
        left = mid + 1
      } else { // go left
        right = mid - 1
      }
    }

    None
  }

}
