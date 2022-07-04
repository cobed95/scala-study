package org.laplacetec.study
package problems99.miscellaneous

object P09 {
  def pack[A <: Equals](acc: List[List[A]], curr: A): List[List[A]] =
    acc match {
      case (leader :: others) :: tail if curr == leader => (curr :: leader :: others) :: tail
      case (head @ _ :: _) :: tail => (curr :: Nil) :: head :: tail
      case _ :: tail => (curr :: Nil) :: tail
      case _ => (curr :: Nil) :: Nil
    }
  /**
   *
   * @tparam A
   * @return
   */
  def solution[A <: Equals]: List[A] => List[List[A]] = l => P08.foldLeftToList(l, pack).reverse
}
