package org.laplacetec.study
package problems99

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
  def last[A]: List[A] => Option[A] = ???

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
  def penultimate[A]: List[A] => Option[A] = ???

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
  def nth[A]: (Int, List[A]) => Option[A] = ???

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
  def length[A]: List[A] => Int = ???

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
  def reverse[A]: List[A] => List[A] = ???

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
  def isPalindrome[A <: Equals]: List[A] => Boolean = ???
  //// start here
  /** (**) Flatten a nested list structure
    *
    * Example: {{{
    *   flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    *   // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    * }}}
    *
    * @return The flattened list.
    */

  def flatten: List[Any] => List[Any] = _ match {
    case Nil => Nil
    case (head: List[_]) :: tail  => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
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
    * 
    *  (0, 1, 2, 2, 2)
    * @tparam A The type of the list's elements.
    * @return The compressed list.
    */
  def compress[A]: List[A] => List[A] = (l: List[A]) => l match {
    case Nil => Nil
    case head :: mid :: tail if head == mid => compress(head::tail)
    case head :: tail => head :: compress(tail)
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
  def _pack[A]: (List[List[A]]) => List[List[A]] = _ match {
    case Nil => Nil
    case head :: mid :: tail if head(0) == mid(0) => _pack((head:::mid) :: tail)
    case head :: tail => head :: _pack(tail)
  }
  def pack[A]: List[A] => List[List[A]] = (l: List[A]) => _pack(l.map(List(_)))

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
  def encode[A]: List[A] => List[(Int, A)] = (l: List[A]) => pack(l).map((s: List[A]) => (s.length, s(0)))
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
  def encodeModified[A]: List[A] => List[Either[A, (Int, A)]] = (l: List[A]) => pack(l).map(
    _ match {
      case s if s.length > 1 => Right((s.length, s(0)))
      case s => Left(s(0))
    }
  )

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
  def decodeHelper[A]: (Int, A) => List[A] = (count, symbol) => {
    if (count == 0) Nil
    else symbol :: decodeHelper(count - 1, symbol)
  }
  def decode[A]: List[(Int, A)] => List[A] = _ match {
    case Nil => Nil
    case (cnt, sym) :: tail => decodeHelper(cnt, sym) ::: decode(tail) 
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
  List(List(1, 2, 3), List(1, 2, 3))
  def encodeDirectHelper[A]: (Int, A, List[A]) => List[(Int, A)] = (cnt, sym, l) => l match{
    case Nil => List((cnt, sym))
    case head :: tail if head == sym => encodeDirectHelper(cnt + 1, sym, tail)
    case head :: tail => (cnt, sym) :: encodeDirectHelper(1, head, tail) 
  }
  def encodeDirect[A]: List[A] => List[(Int, A)] = _ match {
    case Nil => Nil
    case head :: tail => encodeDirectHelper(1, head, tail)
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
  def duplicate[A]: List[A] => List[A] =  {
    case Nil => Nil
    case head :: tail => head :: head :: duplicate(tail)
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

  def duplicateNHelper[A](n: Int, cur: Int, l: List[A]): List[A] = l match {
    case Nil => Nil
    case head :: tail if cur == 0 => duplicateNHelper(n, n, tail)
    case head :: tail => head :: duplicateNHelper(n, cur-1, head::tail)
  }
  def duplicateN[A]: (Int, List[A]) => List[A] = (n, l) => duplicateNHelper(n, n, l)

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
  def dropHelper[A](n: Int, cur: Int, l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: tail if cur == 1 => dropHelper(n, n, tail)
    case head :: tail => head :: dropHelper(n, cur-1, tail) 
  }
  def drop[A]: (Int, List[A]) => List[A] = (n, l) => dropHelper(n, n, l)
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
  def splitHelper[A](n: Int, l: List[A], r: List[A]): (List[A], List[A]) = r match {
    case Nil => (Nil, Nil)
    case head :: tail if n <= 0 => (l.reverse, r) 
    case head :: tail => splitHelper(n-1, head :: l, tail)
  }
  def split[A]: (Int, List[A]) => (List[A], List[A]) = (i: Int, l: List[A]) => splitHelper(i, Nil, l)
}
