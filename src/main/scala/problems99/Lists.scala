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
    case (n, l) if n < 1 => l
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
  def drop[A]: (Int, List[A]) => List[A] = {
    case (_, Nil) => Nil
    case (n, l) if n < 0 => drop(n + length(l), l)
    case (n, _ :: tail) if n == 0 => tail
    case (n, head :: tail) => head :: drop(n - 1, tail)
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
  def putLeftHead[A]: (A, (List[A], List[A])) => (List[A], List[A]) = {
    case (el, (left, right)) => (el :: left, right)
  }
  def split[A]: (Int, List[A]) => (List[A], List[A]) = {
    case (_, Nil) => (Nil, Nil)
    case (n, l) if n < 0 => split(n + length(l), l)
    case (n, head :: tail) if n == 0 => (List(head), tail)
    case (n, head :: tail) => putLeftHead(head, split(n - 1, tail))
  }


  /** P18 (**) Extract a slice from a list.
    * Given two indices, I and K, the slice is the list containing the elements from and including the
    * Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
    *
    * Example: {{{
    *   slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    *   // res0: List[Symbol] = List('d, 'e, 'f, 'g)
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return The slice of the list.
    */
  def slice[A]: (Int, Int, List[A]) => List[A] = ???

  /** P19 (**) Rotate a list N places to the left.
    * Examples: {{{
    *   rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    *   // res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    *
    *   rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    *   // res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    * }}}
    *
    * @tparam A The type of the list's elements.
    * @return The list rotated N places to the left.
    */
  def rotateIter[A]: (Int, List[A], List[A]) => List[A] = {
    case (_, Nil, _) => Nil
    case (n, l, st) if n == 0 => l ::: reverse(st)
    case (n, head :: tail, st) => rotateIter(n-1, tail, head :: st)
  }
  def rotate[A]: (Int, List[A]) => List[A] = {
    case (_, Nil) => Nil
    case (n, l) if n == 0 => l
    case (n, l) if n < 0 => rotate(n + length(l), l)
    case (n, head :: tail) => rotateIter(n-1, tail, List(head))
  }

  /** P20 (*) Remove the Kth element from a list.
    * Return the list and the removed element in a Tuple. Elements are numbered from 0.
    *
    * Example: {{{
    *   removeAt(1, List('a, 'b, 'c, 'd))
    *   // res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return The list with nth element removed and the removed element.
    */
  def removeAt[A]: (Int, List[A]) => (List[A], A) = {
    case (n, l) if n < 0 => removeAt(n + length(l), l)
    case (n, head :: tail) if n == 0 => (tail, head)
    case (n, head :: tail) => removeAt(n - 1, tail) match {
      case (l, el) => (head :: l, el)
    }
  }

  /** P21 (*) Insert an element at a given position into a list.
    *
    * Example: {{{
    *   insertAt('new, 1, List('a, 'b, 'c, 'd))
    *   // res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
    * }}}
    *
    * @tparam A The type of the list's elements
    * @return The list with the new element inserted at the nth position
    */
  def insertAt[A]: (A, Int, List[A]) => List[A] = {
    case (_, _, Nil) => Nil
    case (el, n, l) if n < 0 => insertAt(el, n + length(l), l)
    case (el, n, l) if n == 0 => el :: l
    case (el, n, head :: tail) => head :: insertAt(el, n - 1, tail)
  }

  /** P22 (*) Create a list containing all integers within a given range.
    *
    * Example: {{{
    *   range(4, 9)
    *   // res0: List[Int] = List(4, 5, 6, 7, 8, 9)
    * }}}
    *
    * @return The list of all integers within a given range.
    */
  def rangeIter: (Int, Int, Int) => List[Int] = {
    case (start, stop, _) if start == stop => List(start)
    case (start, stop, step) => start :: rangeIter(start + step, stop, step)
  }
  def range: (Int, Int) => List[Int] = {
    case (start, stop) if start > stop => rangeIter(start, stop, -1)
    case (start, stop) => rangeIter(start, stop, 1)
  }

  /** P23 (**) Extract a given number of randomly selected elements from a list.
    *
    * Example: {{{
    *   randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    *   // res0: List[Symbol] = List('e, 'd, 'a)
    * }}}
    *
    * Hint: Use the solution to problem P20
    *
    * @tparam A The type of the list's elements.
    * @return List with n randomly selected elements from the input list.
    */
  val RANDOM = new scala.util.Random
//  def randRange: (Int, Int) => Int = {
//    case (start, stop) => range(start, stop)
//  }

  def randomSelect[A]: (Int, List[A]) => List[A] = {
    case (_, Nil) => Nil
    case (n, _) if n == 0 => Nil
    case (n, l) => removeAt(RANDOM.nextInt(length(l)), l) match {
      case (resultList, removed) => removed :: randomSelect(n - 1, resultList)
    }
  }

  /** P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    *
    * Example: {{{
    *   lotto(6, 49)
    *   // res0: List[Int] = List(23, 1, 17, 33, 21, 37)
    * }}}
    *
    * @return List of N random numbers from set 1..M.
    */
  def lotto: (Int, Int) => List[Int] = {
    case (cnt, size) => randomSelect(cnt, range(1, size + 1))
  }

  /** P25 (*) Generate a random permutation of the elements of a list.
    * Hint: Use the solution of problem P23.
    *
    * Example:
    *
    * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
    *
    * @tparam A The type of the list's elements.
    * @return The random permutation of the input list.
    */
  def randomPermute[A]: List[A] => List[A] = (l => randomSelect(length(l), l))

  /** P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    *
    * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know
    * that there are C(12, 3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
    * For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
    *
    * @tparam A The type of the list's elements.
    * @return All combinations of K distinct objects from input elements
    */
//  combination([0,1,2,3], 2) = ([0],combination([1,2,3], 1)) + ([1],combination([2,3], 1)) + ([2],combination([3], 1)))

  def combinations[A]: (Int, List[A]) => List[List[A]] = {
    case (_, Nil) => Nil
    case (n, l) if n > length(l) => Nil
    case (n, l) if n == length(l) => List(l)
    case (n, head :: tail) => combinations(n - 1, tail).map(head :: _) ::: combinations(n, tail)
  }

  /** P27 (**) Group the elements of a set into disjoint subsets.
    *
    * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
    * Write a function that generates all the possibilities.
    *
    * Example: {{{
    *   group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    *   // res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
    * }}}
    *
    * b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
    *
    * Example: {{{
    *   group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    *   // res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
    * }}}
    *
    * Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), …) is the same solution as ((Beat, Aldo), …).
    * However, we make a difference between ((Aldo, Beat), (Carla, David), …) and ((Carla, David), (Aldo, Beat), …).
    *
    * You may find more about this combinatorial problem in a good book on discrete mathematics under the term “multinomial coefficients”.
    *
    * ((Carla, David),), ((Aldo, Beat), …)
    * @tparam A The type of the elements of the list to be grouped.
    * @return The list of all possible groups.
    */
//    def diff[A]: (List[A], List[A]) => List[A] = (x, y) => (x.diff(y))

  def group[A]: (List[Int], List[A]) => List[List[List[A]]] = {
    case (Nil, _) => List(Nil)
    case (_, Nil) => List(Nil)
    case (head :: tail, members) => combinations(head, members).flatMap(
      picked => group(tail, members.diff(picked)).map(picked :: _)
    )
  }

  /** P28 (**) Sorting a list of lists according to length of sublists.
    *
    * a) We suppose that a list contains elements that are lists themselves.
    * The objective is to sort the elements of the list according to their length.
    * E.g. short lists first, longer lists later, or vice versa.
    *
    * Example: {{{
    *   lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    *   // res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    * }}}
    *
    * b) Again, we suppose that a list contains elements that are lists themselves.  But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
    *
    * Example: {{{
    *   lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    *   // res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    * }}}
    *
    * Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once.  The third and fourth lists have length 3 and there are two list of this length.  Finally, the last three lists have length 2.  This is the most frequent length.
    *
    * @tparam A The type of the elements of the list.
    * @return The sorted list.
    */
  def lsort[A]: List[List[A]] => List[List[A]] = l => l.sortBy(x => length(x))

  /** P28 (**) Sorting a list of lists according to length of sublists.
    *
    * a) We suppose that a list contains elements that are lists themselves.
    * The objective is to sort the elements of the list according to their length.
    * E.g. short lists first, longer lists later, or vice versa.
    *
    * Example: {{{
    *   lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    *   // res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    * }}}
    *
    * b) Again, we suppose that a list contains elements that are lists themselves.  But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
    *
    * Example: {{{
    *   lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    *   // res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    * }}}
    *
    * Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once.  The third and fourth lists have length 3 and there are two list of this length.  Finally, the last three lists have length 2.  This is the most frequent length.
    *
    * @tparam A The type of the elements of the list.
    * @return The sorted list.
    */
  import scala.collection.mutable.Map
  def lsortFreq[A]: List[List[A]] => List[List[A]] = l => {
    val freqMap = Map[Int,Int]()
    l.foreach(x => {
      val len = length(x)
      freqMap(len) = freqMap.getOrElse(len, 0) + 1
    })
    l.sortBy(x => freqMap.get(length(x)))
  }
}
