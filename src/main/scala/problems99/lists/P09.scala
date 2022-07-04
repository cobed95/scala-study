package org.laplacetec.study
package problems99.lists

object P09 {
  def pack[A <: Equals](acc: List[List[A]], curr: A): List[List[A]] =
    acc match {
      case (leader :: others) :: tail if curr == leader => (curr :: leader :: others) :: tail
      case (head @ _ :: _) :: tail => (curr :: Nil) :: head :: tail
      case _ :: tail => (curr :: Nil) :: tail
      case _ => (curr :: Nil) :: Nil
    }
  /**
   * (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * Example:
   *    scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   * @tparam A
   * @return
   */
  def solution[A <: Equals]: List[A] => List[List[A]] = l => P08.foldLeftToList(l, pack).reverse
}
