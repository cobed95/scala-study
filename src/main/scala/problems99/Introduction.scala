package org.laplacetec.study
package problems99

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Introduction {
  /**
   * The Option Monad
   */
  sealed abstract class LaplaceOption[A]
  case class LaplaceSome[A](value: A) extends LaplaceOption[A]
  case object LaplaceNone extends LaplaceOption

  /**
   * Handling exceptions with the Option Monad
   */
  def someFailableOperation: Int => Option[Int] = ???
  def postProcessing0: Int => Option[Int] = ???
  def postProcessing1: Int => Option[Int] = ???
  def postProcessing2: Int => Option[Int] = ???
  someFailableOperation(0).flatMap(postProcessing0).flatMap(postProcessing1).flatMap(postProcessing2)
  // (Some[T]) getTopScore(T) -> Option[Int]  
  // (None[T])  -> Int  
  /**
   * You can even do this due to the nicely designed language syntax.
   * This is often referred to as the infix notation.
   * Example:
   *    postfix notation: add(1, 1)
   *    infix notation: 1 + 1
   */
  someFailableOperation(0) flatMap postProcessing0 flatMap postProcessing1 flatMap postProcessing2
  // [ Some[Option[Person]], None ]
  // car: Option[Car] -> Some[Car]
  // Some[car].owner: Option[Person]
  // owner.license: Option[String]
  /**
   * We can implement this with pattern matching.
   */
  def laplaceFlatMap[A](opt: Option[A], f: A => Option[A]): Option[A] =
    opt match {
      case Some(v) => f(v)
      case None => None
    }

  /**
   * Try Monad
   */
  def someFailableOperationThatReturnsInt: Int => Int = ???
  Try {
    someFailableOperationThatReturnsInt(0)
  } match {
    case Success(v) => postProcessing0(v)
    case Failure(exception) => println(exception)
  }

  def postProcessingForTry0: Int => Try[Int] = ???
  def postProcessingForTry1: Int => Try[Int] = ???
  def postProcessingForTry2: Int => Try[Int] = ???
  Try(someFailableOperationThatReturnsInt(0)).flatMap(postProcessingForTry0).flatMap(postProcessingForTry1).flatMap(postProcessingForTry2)

  Try {
    someFailableOperationThatReturnsInt(0)
  } flatMap postProcessingForTry0 flatMap postProcessingForTry1 flatMap postProcessingForTry2

  Try {
    someFailableOperationThatReturnsInt(0)
  } flatMap {
    postProcessingForTry0
  } flatMap {
    postProcessingForTry1
  } flatMap {
    postProcessingForTry2
  }

  /**
   * Pattern matching can be performed on Algebraic Data Types, or ADT's
   */
  sealed abstract class LaplaceList[A]
  case class LaplaceCons[A](head: A, tail: LaplaceList[A]) extends LaplaceList[A]
  case object LaplaceNil extends LaplaceList

  @tailrec
  def printEachElementInLaplaceList[A](ll: LaplaceList[A]): Unit =
    ll match {
      case LaplaceCons(head, tail) =>
        println(head)
        printEachElementInLaplaceList(tail)
      case LaplaceNil => ()
    }

  /**
   * Scala's stdlib List has a nice overloaded operator builtin for us.
   */
  @tailrec
  def printEachElementInList0[A](l: List[A]): Unit =
    l match {
      case head :: tail =>
        println(head)
        printEachElementInList0(tail)
      case Nil => ()
    }

  /**
   * The 'rest' case can be represented by _
   */
  @tailrec
  def printEachElementInList1[A](l: List[A]): Unit =
    l match {
      case head :: tail =>
        println(head)
        printEachElementInList1(tail)
      case _ => ()
    }

  /**
   * Pattern matching can also be combined with conditional branching.
   */
  def getLarger(a: Int, b: Int): Int =
    (a, b) match {
      // Destructured values can be ignored with _ as well.
      case (_, _) if a > b => a
      case _ => b
    }
}
