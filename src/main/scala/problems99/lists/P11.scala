package org.laplacetec.study
package problems99.lists

object P11 {
  def encodeModified[A <: Equals](acc: List[Either[A, (Int, A)]], curr: A): List[Either[A, (Int, A)]] =
    acc match {
      case Right((cnt, comp)) :: tail if curr == comp => Right(cnt + 1, comp) :: tail
      case Right((_, _)) :: _ => Left(curr) :: acc
      case Left(comp) :: tail if curr == comp => Right(2, comp) :: tail
      case Left(_) :: _ => Left(curr) :: acc
      case _ => Left(curr) :: Nil
    }
  /**
   * (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates
   * it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   * Example:
   *    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   * @tparam A
   * @return
   */
  def solution[A <: Equals]: List[A] => List[Either[A, (Int, A)]] = l => P08.foldLeftToList(l, encodeModified).reverse
}
