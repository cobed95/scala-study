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
  def last[A]: List[A] => Option[A] = {
    case Nil       => None
    case x :: Nil  => Some(x)
    case _ :: rest => last(rest)
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
    case Nil           => None
    case x :: Nil      => None
    case x :: y :: Nil => Some(x)
    case _ :: rest     => penultimate(rest)
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
    case (_, Nil)       => None
    case (0, x :: rest) => Some(x)
    case (n, x :: rest) => nth(n - 1, rest)
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
    case Nil       => 0
    case x :: Nil  => 1
    case x :: rest => length(rest) + 1
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
    case x :: Nil  => List(x)
    case x :: rest => reverse(rest) ::: List(x)
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
  def isPalindrome[A]: List[A] => Boolean = x => x == reverse(x)

  /** (**) Flatten a nested list structure
    *
    * Example: {{{
    *   flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    *   // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    * }}}
    *
    * @return The flattened list.
    */
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case x: List[_] => flatten(x)
    case x          => List(x)
  }

  def flatMap[A, B](ls: List[A], f: A => List[B]): List[B] = ls match {
    case x :: rest => f(x) ::: flatMap(rest, f)
    case _         => Nil
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
  def compress[A]: List[A] => List[A] = { ls =>
    fold[A, List[A]](ls, List(), {
      case (Nil, a)              => List(a)
      case (b, a) if b.last == a => b
      case (b, a)                => b ::: List(a)
    })
  }

  private def fold[A, B](ls: List[A], i: B, f: (B, A) => B): B = ls match {
    case x :: rest => f(fold(rest, i, f), x)
    case _         => i
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
  def pack[A]: List[A] => List[List[A]] = { ls =>
    fold[A, List[List[A]]](ls, List(List()), {
      case (List(Nil), a)                  => List(List(a))
      case ((x :: y) :: rest, a) if x == a => (a :: x :: y) :: rest
      case (b, a)                          => List(a) :: b
    })
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
  def encode[A]: List[A] => List[(Int, A)] = { ls =>
    map[List[A], (Int, A)](pack(ls), {
      case x :: rest => (length(x :: rest), x)
    })
  }

  def map[A, B](ls: List[A], f: A => B): List[B] = ls match {
    case x :: rest => f(x) :: map(rest, f)
    case _         => Nil
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
  def encodeModified[A]: List[A] => List[Either[A, (Int, A)]] = { ls =>
    map[(Int, A), Either[A, (Int, A)]](encode(ls), {
      case (x, y) if x == 1 => Left(y)
      case (x, y)           => Right((x, y))
    })
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
  def decode[A]: List[(Int, A)] => List[A] = { ls =>
    fold[(Int, A), List[A]](ls, List(), {
      case (b, (x, y)) if x > 0 => y :: decode(List((x - 1, y))) ::: b
      case (b, _)               => b
    })
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
  def encodeDirect[A]: List[A] => List[(Int, A)] = { ls =>
    fold[A, List[(Int, A)]](ls, List(), {
      case (Nil, a)                      => List((1, a))
      case ((x, y) :: rest, a) if y == a => (x + 1, y) :: rest
      case (b, a)                        => (1, a) :: b
    })
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
  def duplicate[A]: List[A] => List[A] = { ls =>
    fold[A, List[A]](ls, List(), {
      case (b, a) => a :: a :: b
    })
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
  def duplicateN[A]: (Int, List[A]) => List[A] = { (x, ls) =>
    fold[A, List[A]](ls, List(), {
      case (b, a) if x > 0 => a :: duplicateN(x - 1, List(a)) ::: b
      case _               => Nil
    })
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
  def drop[A]: (Int, List[A]) => List[A] = { (x, ls) =>
    fold[A, (Int, Int, List[A])](ls, (x, 1, List()), {
      case ((x, y, z), a) if x == y => (x, 1, z)
      case ((x, y, z), a)           => (x, y + 1, a :: z)
    })._3
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
  def split[A]: (Int, List[A]) => (List[A], List[A]) = { (x, ls) =>
    fold[A, (Int, Int, (List[A], List[A]))](ls, (x, 0, (List(), List())), {
      case ((x, y, (z1, z2)), a) if y < length(ls) - x =>
        (x, y + 1, (z1, a :: z2))
      case ((x, y, (z1, z2)), a) => (x, y + 1, (a :: z1, z2))
    })._3
  }

  def slice[A]: (Int, Int, List[A]) => List[A] = { (x1, x2, ls) =>
    fold[A, (Int, List[A])](ls, (0, List()), {
      case ((x, z), a) if x1 <= x && x < x2 => (x + 1, a :: z)
      case ((x, z), a)                      => (x + 1, z)
    })._2
  }

  def rotate[A]: (Int, List[A]) => List[A] = {
    case (x, ls) if x > 0 =>
      split(x, ls) match {
        case (ls1, ls2) => ls2 ::: ls1
      }
    case (x, ls) =>
      split(length(ls) + x, ls) match {
        case (ls1, ls2) => ls2 ::: ls1
      }
  }

  def removeAt[A]: (Int, List[A]) => (List[A], A) = { (x, ls) =>
    split(x, ls) match {
      case (ls1, y :: ls2) => (ls1 ::: ls2, y)
    }
  }

  def insertAt[A]: (A, Int, List[A]) => List[A] = { (x, y, ls) =>
    split(y, ls) match {
      case (ls1, ls2) => ls1 ::: x :: ls2
    }
  }

  def range[A]: (Int, Int) => List[Int] = {
    case (x1, x2) if x1 == x2 => List(x2)
    case (x1, x2) if x1 < x2  => x1 :: range(x1 + 1, x2)
    case _                    => Nil
  }
}
