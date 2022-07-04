package org.laplacetec.study
package problems99.lists

object P07 {
  type NestedList[A] = List[Either[A, NestedList[A]]]
  /**
   * (**) Flatten a nested list structure.
   * Example:
   *    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   *    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   * @return
   */
  def solution[A]: NestedList[A] => List[A] = {
    case Left(v) :: tl => v :: solution(tl)
    case Right(li) :: tl => solution(li) ++ solution(tl)
    case _ => Nil
  }
}
