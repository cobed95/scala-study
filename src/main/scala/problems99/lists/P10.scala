package org.laplacetec.study
package problems99.lists

object P10 {
  def encode[A <: Equals](acc: List[(Int, A)], curr: A): List[(Int, A)] =
    acc match {
      case (cnt, comp) :: tail if curr == comp => (cnt + 1, curr) :: tail
      case _ => (1, curr) :: acc
    }
  /**
   * (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
   * Consecutive duplicates of elements are encoded as tuples (N, E)
   * where N is the number of duplicates of the element E.
   * Example:
   *    scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   * @tparam A
   * @return
   */
  def solution[A <: Equals]: List[A] => List[(Int, A)] = l => P08.foldLeftToList(l, encode).reverse
}
