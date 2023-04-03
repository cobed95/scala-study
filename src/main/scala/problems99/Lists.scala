package org.laplacetec.study
package problems99

import scala.annotation.tailrec
import scala.util.Random
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
  def split[A]: (Int, List[A]) => (List[A], List[A]) = (n, ls) => {
    if (n < 0) split(n + length(ls), ls)
    else ls.foldLeft((n, List[A](), List[A]())) {
      case ((n, l, r), hd) if n <= 0 => (n, l, hd :: r)
      case ((n, l, r), hd) => (n - 1, hd :: l, r)
    } match {
      case (_, l, r) => (reverse(l), reverse(r))
    }
  }

  def switchConcat[A]: (List[A], List[A]) => List[A] = (l, r) => r ::: l

  def untuple[A, B, C]: (((A, B)) => C) => (A, B) => C = f => (a, b) => f.apply((a, b))

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
  def slice[A]: (Int, Int, List[A]) => List[A] = {
    case (_, _, Nil) => Nil
    case (i, j, _ :: _) if i >= j => Nil
    case (i, j, _ :: tl) if i > 0 => slice(i - 1, j - 1, tl)
    case (i, j, hd :: tl) => hd :: slice(i, j - 1, tl)
  }

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
  def rotate[A]: (Int, List[A]) => List[A] = untuple(split[A].tupled andThen switchConcat[A].tupled)

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
    case (_, Nil) => throw new ArrayIndexOutOfBoundsException
    case (n, hd :: tl) if n <= 0 => (tl, hd)
    case (n, hd :: tl) => removeAt(n - 1, tl) match {
      case (remaining, el) => (hd :: remaining, el)
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
    case (el, _, Nil) => el :: Nil
    case (el, n, ls) if n <= 0 => el :: ls
    case (el, n, hd :: tl) => hd :: insertAt(el, n - 1, tl)
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
  def range: (Int, Int) => List[Int] = {
    case (n, m) if n > m => Nil
    case (n, m) => n :: range(n + 1, m)
  }

  val rand = new Random

  def randomSelectIter[A]: Int => (Int, List[A]) => List[A] = length => (n, ls) => {
    (length, n, ls) match {
      case (_, _, Nil) => Nil
      case (l, m, _ :: _) if m <= 0 => Nil
      case (l, m, _ :: _) => removeAt(rand.nextInt(l), ls) match {
        case (remaining, removed) => removed :: randomSelectIter(l - 1)(m - 1, remaining)
      }
    }
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
  def randomSelect[A]: (Int, List[A]) => List[A] = (n, ls) => randomSelectIter(length(ls))(n, ls)

  /** P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    *
    * Example: {{{
    *   lotto(6, 49)
    *   // res0: List[Int] = List(23, 1, 17, 33, 21, 37)
    * }}}
    *
    * @return List of N random numbers from set 1..M.
    */
  def lotto: (Int, Int) => List[Int] = (n, m) => randomSelect(n, range(1, m))

//  def applySequentially[A, B, C]: (A, B, A => B, B => C) => C = (a, b, f, g) =>
//  def lotto2: (Int, Int) => List[Int] = range.curried(1) andThen randomSelect

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
  def randomPermute[A]: List[A] => List[A] = ls => randomSelect(length(ls), ls)

  def identity[A]: A => A = x => x
  def apply2[A, B, C]: (A => B) => (A => C) => A => (B, C) = f => g => a => (f(a), g(a))
  def lengthAndSelf[A]: List[A] => (Int, List[A]) = apply2(length[A])(identity[List[A]])
  def randomPermute2[A]: List[A] => List[A] = lengthAndSelf andThen randomSelect.tupled

  /** P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    *
    * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know
    * that there are C(12, 3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
    * For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
    *
    * @tparam A The type of the list's elements.
    * @return All combinations of K distinct objects from input elements
    */
  def combinations[A]: (Int, List[A]) => List[List[A]] = {
    case (n, _) if n <= 0 => Nil
    case (n, ls) if n == 1 => ls map (List(_))
    case (_, Nil) => Nil
    case (n, hd :: tl) => (combinations(n - 1, tl) map (hd :: _)) ::: combinations(n, tl)
  }

  implicit class Crossable[X](xs: Iterable[X]) {
    def cross[Y](ys: Iterable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  implicit class Listifyable[X](xs: Iterable[(X, X)]) {
    def listify = xs map { case (x, y) => List(x, y) }
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
    * @tparam A The type of the elements of the list to be grouped.
    * @return The list of all possible groups.
    */
  def group[A]: (List[Int], List[A]) => List[List[List[A]]] = {
    case (groupSizes, toGroup) if groupSizes.sum != length(toGroup) => Nil
    case (Nil, toGroup) => Nil
    case (hd :: tl, toGroup) => combinations(hd, toGroup) map (List[List[A]](_))
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
  def lsort[A]: List[List[A]] => List[List[A]] = ???

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
  def lsortFreq[A]: List[List[A]] => List[List[A]] = ???
}
