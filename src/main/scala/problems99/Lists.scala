package org.laplacetec.study
package problems99

import scala.annotation.tailrec

object Lists {
  /** (*) Find the last element of a list
    *
    * Example: {{{
    *   last(List(1, 1, 2, 3, 5, 8))
    *   // res0: Int = 8
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return Some(The last element) of a list if it exists, else None
    */

  def last[A]: List[A] => Option[A] = {
    case head :: Nil => Some(head)
    case _ :: tail => last(tail)
    case Nil => None
  }
  /** (*) Find the last but one element of a list
    *
    * Example: {{{
    *   penultimate(List(1, 1, 2, 3, 5, 8))
    *   // res0: Int = 5
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return Some(penultimate element) if it exists, None otherwise.
    */
  def penultimate[A]: List[A] => Option[A] = {
    case head :: _ :: Nil => Some(head)
    case _ :: tail => penultimate(tail)
    case Nil => None
  }

  /** (*) Find the Kth element of a list.
    *
    * By convention, the first element in the list is element 0.
    * Example: {{{
    *   nth(2, List(1, 1, 2, 3, 5, 8))
    *   // res0: Int = 2
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return Some(nth element) if it exists, None otherwise
    */
  @tailrec
  def nth[A](idx: Int, lst: List[A]): Option[A] = {
    lst match {
      case head :: _ if (idx == 0) => Some(head)
      case _ :: tail => nth(idx - 1, tail)
      case Nil => None
      case _ if (idx < 0) => None
    }
  }

  def nth2[A]: (Int, List[A]) => Option[A] = (idx: Int, lst: List[A]) => {
    lst match {
      case head :: _ if (idx == 0) => Some(head)
      case _ :: tail => nth2(idx - 1, tail)
      case Nil => None
      case _ if (idx < 0) => None
    }
  }

  def nth3[A]: (Int, List[A]) => Option[A] = {
    case (n, _ :: tail) => nth3(n - 1, tail)
    case (0, head :: _) => Some(head)
    case (_, Nil) => None
  }

  /** (*) Find the number of elements of a list.
    *
    * Example: {{{
    *   length(List(1, 1, 2, 3, 5, 8))
    *   // res0: Int = 6
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return The number of elements in the list.
    */
  def length[A]: List[A] => Int = {
    case _ :: tail => 1 + length(tail)
    case Nil => 0
  }

  /** (*) Reverse a list.
    *
    * Example: {{{
    *   reverse(List(1, 1, 2, 3, 5, 8))
    *   // res0: List[Int] = List(8, 5, 3, 2, 1, 1)
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The reversed list.
    */
  def reverse[A]: List[A] => List[A] = {
    case Nil => Nil
    case head :: tail => reverse(tail).appended(head)
//    case head :: tail => reverse(tail) ::: List(head)
  }

  /** (*) Find out whether a list is a palindrome.
    *
    * Example: {{{
    *   isPalindrome(List(1, 2, 3, 2, 1))
    *   // res0: Boolean = true
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return True if the list is a palindrome, false otherwise
    */
  def isPalindrome[A]: List[A] => Boolean = l => l == reverse(l)

  /** (**) Flatten a nested list structure
    *
    * Example: {{{
    *   flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    *   // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    * }}}
    *
    * @return The flattened list.
    */
  def flatten: List[Any] => List[Any] = {
    case Nil => Nil
    case head :: tail => head match {
      case _head: List[Any] => flatten(_head) ::: flatten(tail)
      case _head => _head :: flatten(tail)
    }
  }

  /** (**) Eliminate consecutive duplicates of list elements.
    *
    * If a list contains repeated elements they should be replaced with a single copy of the element.
    * The order of the elements should not be changed.
    * Example: {{{
    *   compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    *   // res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The compressed list.
    */

  def _compress[A]: (List[A], List[A]) => List[A] = {
    case (Nil, l) =>l
    case (head :: tail, Nil) => _compress(tail, List(head))
    case (head :: tail, head2 :: tail2) if head == head2 => _compress(tail, head2 :: tail2)
    case (head :: tail, l) => _compress(tail, head :: l)
  }

  def compress[A]: List[A] => List[A] = {
    a => reverse(_compress(a, Nil))
  }

  /** (**) Pack consecutive duplicates of list elements into sublists.
    *
    * If a list contains repeated elements they should be placed in separate sublists.
    * Example: {{{
    *   pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    *   // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return A new list where consecutive repeated elements are packed into sublists.
    */
  def _pack[A]: (List[A], List[A], List[List[A]]) => List[List[A]] = {
    case (head :: tail, Nil, res) => _pack(tail, List(head), res)
    case (Nil, Nil, res) => res
    case (Nil, tmp, res) => _pack(Nil, Nil, tmp :: res)
    case (head :: tail, head2 :: tail2, l) if head == head2 => _pack(tail, head :: head2 :: tail2, l)
    case (head :: tail, tmp, res) => _pack(tail, List(head), tmp :: res)
  }

  def pack[A]: List[A] => List[List[A]] = {
    a => reverse(_pack(a, Nil, Nil))
  }

  /** (*) Run-length encoding of a list.
    *
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
    * Consecutive duplicates of elements are encoded as tuples (N, E)
    * where N is the number of duplicates of the element E.
    * Example: {{{
    *   encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    *   // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return A new run-length encoded list.
    */

  def encodeIter[A]: (List[A], Int, A, List[(Int, A)]) => List[(Int, A)] = {
    case (head :: tail, 0, _, res) => encodeIter(tail, 1, head, res)
    case (Nil, 0, _, res) => res
    case (Nil, len, latest, res) => encodeIter(Nil, 0, latest, (len, latest) :: res)
    case (head :: tail, len, latest, res) if head == latest => encodeIter(tail, len + 1, latest, res)
    case (head :: tail, len, latest, res) => encodeIter(tail, 1, head, (len, latest):: res)
  }

  def encode[A]: List[A] => List[(Int, A)] = {
    a => reverse(encodeIter(a, 0, a.head, Nil))
  }

  /** (*) Modified run-length encoding.
    *
    * Modify the result of problem P10 in such a way that if an element has no duplicates
    * it is simply copied into the result list.
    * Only elements with duplicates are transferred as (N, E) terms.
    * Example: {{{
    *   encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    *   // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return A new run-length encoded list with elements with no duplicates simply copied.
    */
  def encodeModifiedIter[A]: (List[A], List[Either[A, (Int, A)]]) => List[Either[A, (Int, A)]] = {
    case (left_head :: left_tail, Right((right_head_len, right_head_el)) :: right_tail) if left_head == right_head_el => encodeModifiedIter(left_tail, Right((right_head_len + 1, right_head_el)) :: right_tail)
    case (left_head :: left_tail, Left(right_head) :: right_tail) if left_head == right_head  => encodeModifiedIter(left_tail, Right((2, right_head)) :: right_tail)
    case (left_head :: left_tail, right) => encodeModifiedIter(left_tail, Left(left_head) :: right)
    case (Nil, right) => right
  }

  // List(Right((4,a)), Left(b), Right((2,c)), Right((2,a)), Left(d), Right((4,e))) ????
  def encodeModified[A]: List[A] => List[Either[A, (Int, A)]] = {
    case head :: tail => reverse(encodeModifiedIter(tail, List(Left(head))))
    case Nil => Nil
  }

  /** (**) Decode a run-length encoded list.
    *
    * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    * Example: {{{
    *   decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    *   // res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return A new list that contains the decoded result.
    */
  def decode[A]: List[(Int, A)] => List[A] = {
    case Nil => Nil
    case (len, el) :: Nil => fill(len, el)
    case (len, el) :: tail => fill(len, el) ::: decode(tail)
  }

  /** (**) Run-length encoding of a list (direct solution).
    *
    * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    * Example: {{{
    *   encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    *   // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The directly encoded result stored in a list.
    */

  def fill[A]: (Int, A) => List[A] = {
    case (len, el) if len > 0 => el :: fill(len - 1, el)
    case _ => Nil
  }

  def encodeDirectIter[A]: (List[A], List[(Int, A)]) => List[(Int, A)] = {
    case (left_head :: left_tail, (right_head_len, right_head_el) :: right_tail) if left_head == right_head_el => encodeDirectIter(left_tail, (right_head_len + 1, right_head_el) :: right_tail)
    case (left_head :: left_tail, right) => encodeDirectIter(left_tail, (1, left_head) :: right)
    case (Nil, right) => right
  }

  def encodeDirect[A]: List[A] => List[(Int, A)] = {
    case Nil => Nil
    case head :: tail => reverse(encodeDirectIter(tail, List((1, head))))
  }

  /** (*) Duplicate the elements of a list.
    *
    * Example: {{{
    *   duplicate(List('a, 'b, 'c, 'c, 'd))
    *   // res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return The list with duplicated elements.
    */

  def duplicate[A]: List[A] => List[A] = {
    case Nil => Nil
    case head :: Nil => List.fill(2)(head)
    case head :: tail => List.fill(2)(head) ::: duplicate(tail)
  }

  /** (**) Duplicate the elements of a li7st a given number of times.
    *
    * Example: {{{
    *   duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    *   //  res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The list with n duplicates.
    */
  def duplicateN[A]: (Int, List[A]) => List[A] = {
    case (_, Nil) => Nil
    case (n, head :: Nil) => List.fill(n)(head)
    case (n, head :: tail) => List.fill(n)(head) ::: duplicateN(n, tail)
  }

  /** (**) Drop every Nth element from a list.
    *
    * Example: {{{
    *   drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    *   // res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The list with every Nth element dropped.
    */
  def drop[A]: (Int, List[A]) => List[A] = ???

  /** (*) Split a list into two parts.
    *
    * The length of the first part is given. Use a Tuple for your result.
    * Example: {{{
    *   split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    *   // res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return Tuple of two lists where the first list's length is n, and the second list's length is list.length - n
    */
  def split[A]: (Int, List[A]) => (List[A], List[A]) = ???
}
