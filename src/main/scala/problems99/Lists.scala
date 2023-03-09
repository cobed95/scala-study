package org.laplacetec.study
package problems99

import scala.List
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
    case head :: tail if tail.isEmpty => Some(head)
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
    case head :: tail if tail.length == 1 => Some(head)
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
  def nth[A](n: Int, ls: List[A]): Option[A] = {
    if(ls.isEmpty) None
    else if(n == 0) Some(ls.head)
    else ls match {
      case Nil => None
      case _ :: tail => nth(n - 1, tail)
    }
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
    case head :: tail => reverse(tail) ::: List(head)
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
  def isPalindrome[A](ls: List[A]): Boolean = ls == reverse(ls)

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
    case (head: List[Any]) :: Nil => flatten(head)
    case ls @ _ :: Nil => ls
    case head :: tail => flatten(List(head)) ::: flatten(tail)
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
  def compress[A]: List[A] => List[A] = {
    case Nil => Nil
    case head :: Nil => List(head)
    case head :: tail if head != tail.head => compress(List(head)) ::: compress(tail)
    case _ :: tail => compress(tail)
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
  def packIter[A]: (List[A], List[A]) => List[List[A]] = {
    case (Nil, Nil) => Nil
    case (ls, Nil) => ls :: Nil
    case (res @ hdRes :: _, hdIter :: tlIter) if hdRes == hdIter => packIter(hdIter :: res, tlIter)
    case (res @ _ :: _, hdIter :: tlIter) => res :: packIter(hdIter :: Nil, tlIter)
  }

  def pack[A]: List[A] => List[List[A]] = {
    case Nil => Nil
    case hd :: tl => packIter(hd :: Nil, tl)
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

  def filterNoneAndGet[A]: List[(Int, Option[A])] => List[(Int, A)] = {
    case Nil => Nil
    case (_, None) :: tl => filterNoneAndGet(tl)
    case (n, Some(v)) :: tl => (n, v) :: filterNoneAndGet(tl)
  }

  def encode[A](ls: List[A]): List[(Int, A)] = filterNoneAndGet(pack(ls) map {
    case Nil => (0, None)
    case packed @ hd :: _ => (length(packed), Some(hd))
  })

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
  def encodeModified[A](ls: List[A]): List[Either[A, (Int, A)]] = encode(ls) map {
    case (len, el) => if (len == 1) Left(el) else Right(len, el)
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
    case (len, el) :: tl if len != 0 => el :: decode((len - 1, el) :: tl)
    case _ :: Nil => Nil
    case _ :: tl => decode(tl)
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
  def dataCompression[A]: ((Int, A), List[A]) => List[(Int, A)] = {
    case (res, Nil) => res :: Nil
//    case ((len, hdRes), hdIter :: Nil) if hdRes == hdIter => dataCompression((len + 1, hdRes), Nil)
//    case (res, hdIter :: Nil) => res :: dataCompression((1, hdIter), Nil)
    case ((len, hdRes), hdIter :: tlIter) if hdRes == hdIter => dataCompression((len + 1, hdRes), tlIter)
    case (res, hdIter :: tlIter) => res :: dataCompression((1, hdIter), tlIter)
  }

  def encodeDirect[A]: List[A] => List[(Int, A)] = {
    case hd :: tl => dataCompression((1, hd), tl)
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
    case hd :: tl => hd :: hd :: duplicate(tl)
  }

  /**
    * P18 (**) Extract a slice from a list.
    * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to
    * but not including the Kth element of the original list. Start counting the elements with 0.
    *
    * Example:
    * scala> slice(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'), 3, 7)
    * res0: List[Char] = List(d, e, f, g)
    *
    * @param list the list to extract a slice from
    * @param start the index of the first element to include in the slice
    * @param end the index of the first element to exclude from the slice
    * @tparam A the type of elements in the list
    * @return the slice of the original list
    */
  def slice[A](list: List[A], start: Int, end: Int): List[A] = list match {
    case hd :: tl => sliceIter((0, hd), tl, start, end)
  }

  def sliceIter[A]: ((Int, A), List[A], Int, Int) => List[A] = {
    case ((idx, _), hdIter :: tlIter, start, end) if idx < start => sliceIter((idx + 1, hdIter), tlIter, start, end)
    case ((idx, _), _, _, end) if idx >= end => Nil
    case ((idx, hd), hdIter :: tl, start, end) => hd :: sliceIter((idx + 1, hdIter), tl, start, end)
  }

  /**
    * P19 (**) Rotate a list N places to the left.
    * Examples:
    * scala> rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))
    * res0: List[Char] = List(d, e, f, g, h, a, b, c)
    *
    * scala> rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))
    * res1: List[Char] = List(g, h, a, b, c, d, e, f)
    */
  def rotate[A](n: Int, ls: List[A]): List[A] = ls match {
    case hd :: tl if n >= 0 => rotateIter(hd :: Nil, tl, n - 1)
    case hd :: tl => rotateIter(hd :: Nil, tl, tl.length + n)
  }

  def rotateIter[A]: (List[A], List[A], Int) => List[A] = {
    case (stack, hd :: tl, n) if n != 0 => rotateIter(hd :: stack, tl, n - 1)
    case (stack, ls, _) => ls ::: reverse(stack)
  }

  /**
    * Removes the element at the given index from a list.
    * Returns a tuple containing the element removed and the resulting list.
    *
    * scala> removeAt(1, List('a, 'b, 'c, 'd))
    * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
    *
    * @param n the index of the element to remove
    * @param list the list to remove the element from
    * @tparam A the type of elements in the list
    * @return a tuple containing the element removed and the resulting list
    */
  def removeAt[A](n: Int, list: List[A]): (A, List[A]) = list match {
    case hd :: tl if n == 0 => (hd, tl)
    case hd :: tl => removeAtIter(n - 1, tl, hd :: Nil)
  }

  def removeAtIter[A]: (Int, List[A], List[A]) => (A, List[A]) = {
    case (n, hd :: tl, stack) if n == 0 => (hd, reverse(stack) ::: tl)
    case (n, hd :: tl, stack) => removeAtIter(n - 1, tl, hd :: stack)
  }

  /**
    * Inserts an element at the given position in a list.
    *
    * @param elem the element to insert
    * @param n the position to insert the element at (0-indexed)
    * @param list the list to insert the element into
    * @tparam A the type of elements in the list
    * @return the resulting list after the element has been inserted
    */
  def insertAt[A](elem: A, n: Int, list: List[A]): List[A] = list match {
    case ls if n == 0 => elem :: ls
    case hd :: tl => hd :: insertAt(elem, n - 1, tl)
  }

  /**
    * Generates a list of integers from a starting value to an ending value (inclusive).
    *
    * @param start the starting value of the range
    * @param end the ending value of the range
    * @return a list of integers from `start` to `end` (inclusive)
    */
  def range(start: Int, end: Int): List[Int] = {
    if (start > end) return Nil
    start :: range(start + 1, end)
  }
}
