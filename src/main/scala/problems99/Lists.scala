package org.laplacetec.study
package problems99

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

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
    case hd :: Nil => Some(hd)
    case _ :: tl => last(tl)
    case _ => None
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
    case hd :: _ :: Nil => Some(hd)
    case _ :: Nil => None
    case _ :: tl => penultimate(tl)
    case _ => None
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
  def nth[A]: (Int, List[A]) => Option[A] = {
    case (0, hd :: _) => Some(hd)
    case (n, _ :: tl) if n > 0 => nth(n - 1, tl)
    case (_, _ :: tl) => None
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
    case Nil => 0
    case _ :: tl => length(tl) + 1
  }

  @tailrec
  def reverseIter[A](left: List[A])(right: List[A]): List[A] = right match {
    case hd :: tl => reverseIter(hd :: left)(tl)
    case _ => left
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
  def reverse[A]: List[A] => List[A] = reverseIter(Nil)

  def zip[L, R](left: List[L])(right: List[R]): List[(L, R)] = (left, right) match {
    case (hdLeft :: tlLeft, hdRight :: tlRight) => (hdLeft, hdRight) :: zip(tlLeft)(tlRight)
    case (_, Nil) => Nil
    case (_, _) => Nil
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
  def isPalindrome[A <: Equals]: List[A] => Boolean = ls => (ls zip reverse(ls)) forall {
    case (a, b) => a == b
  }

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
    case hd :: tl => hd match {
      case ns @ _ :: _ => flatten(ns.asInstanceOf[List[Any]]) ::: flatten(tl)
      case _ => hd :: flatten(tl)
    }
  }

  def compressIter[A]: (A, List[A]) => List[A] = {
    case (dup, Nil) => dup :: Nil
    case (dup, hd :: tl) if dup == hd => compressIter(dup, tl)
    case (dup, hd :: tl) => dup :: compressIter(hd, tl)
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
    case hd :: tl => compressIter(hd, tl)
  }

  def packIter[A]: (List[A], List[A]) => List[List[A]] = {
    case (Nil, Nil) => Nil
    case (ls, Nil) => ls :: Nil
    case (res @ hdRes :: _, hdIter :: tlIter) if hdRes == hdIter => packIter(hdIter :: res, tlIter)
    case (res @ _ :: _, hdIter :: tlIter) => res :: packIter(hdIter :: Nil, tlIter)
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
  def pack[A <: Equals]: List[A] => List[List[A]] = {
    case Nil => Nil
    case hd :: tl => packIter(hd :: Nil, tl)
  }

  def filterNoneAndGet[A <: Equals]: List[(Int, Option[A])] => List[(Int, A)] = {
    case Nil => Nil
    case (_, None) :: tl => filterNoneAndGet(tl)
    case (n, Some(v)) :: tl => (n, v) :: filterNoneAndGet(tl)
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
  def encode[A <: Equals](ls: List[A]): List[(Int, A)] = filterNoneAndGet(pack(ls) map {
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
  def encodeModified[A <: Equals]: List[A] => List[Either[A, (Int, A)]] = ls => encode(ls) map {
    case (n, el) if n == 1 => Left(el)
    case el => Right(el)
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
    case (n, _) :: tl if n <= 0 => decode(tl)
    case (n, el) :: tl => el :: decode((n - 1, el) :: tl)
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
  def encodeDirect[A]: List[A] => List[(Int, A)] = ???

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

  def duplicateNIter[A](remaining: Int)(shouldRepeat: Int, ls: List[A]): List[A] =
    (remaining, shouldRepeat, ls) match {
      case (_, s, _) if s <= 0 => ls
      case (_, _, Nil) => Nil
      case (r, _, _ :: tl) if r <= 0 => duplicateNIter(shouldRepeat)(shouldRepeat, tl)
      case (r, s, hd :: _) => hd :: duplicateNIter(r - 1)(s, ls)
    }

  /** (**) Duplicate the elements of a list a given number of times.
    *
    * Example: {{{
    *   duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    *   //  res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The list with n duplicates.
    */
  def duplicateN[A]: (Int, List[A]) => List[A] = (n, ls) => duplicateNIter(n)(n, ls)

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
  def drop[A]: (Int, List[A]) => List[A] = (n, ls) => ls.foldLeft((1, List[A]())) {
    case ((m, acc), _) if m == n => (1, acc)
    case ((m, acc), hd) => (m + 1, hd :: acc)
  } match {
    case (_, acc) => reverse(acc)
  }


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
  def split[A]: (Int, List[A]) => (List[A], List[A]) = (n, ls) => ls.foldLeft((n, List[A](), List[A]())) {
    case ((n, l, r), hd) if n <= 0 => (n, l, hd :: r)
    case ((n, l, r), hd) => (n - 1, hd :: l, r)
  } match {
    case (_, l, r) => (reverse(l), reverse(r))
  }
}
