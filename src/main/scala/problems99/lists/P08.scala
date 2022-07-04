package org.laplacetec.study
package problems99.lists

object P08 {
  def foldLeftToList[A <: Equals, B](l: List[A], f: (List[B], A) => List[B]): List[B] = (l foldLeft[List[B]] Nil)(f)
  def compress[A <: Equals](acc: List[A], curr: A): List[A] =
    acc match {
      case head :: _ if head == curr => acc
      case _ :: _ => curr :: acc
      case _ => curr :: Nil
    }
  /**
   * (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   * Example:
   *    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   * @return
   */
  def solution[A <: Equals]: List[A] => List[A] = l => foldLeftToList(l, compress).reverse
}
