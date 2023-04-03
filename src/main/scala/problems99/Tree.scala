package org.laplacetec.study
package problems99

import scala.math.Ordering.Implicits.infixOrderingOps

/** A binary tree is either empty or it is composed of a root element and two successors,
  * which are binary trees themselves. We shall use the following classes to represent binary trees.
  * (Also available in tree1.scala.) An End is equivalent to an empty tree. A Branch has a value,
  * and two descendant trees. The toString functions are relatively arbitrary, but they yield
  * a more compact output than Scala’s default. Putting a plus in front of the T makes the class covariant;
  * it will be able to hold subtypes of whatever type it’s created for.
  * (This is important so that End can be a singleton object; as a singleton, it must have a specific type,
  * so we give it type Nothing, which is a subtype of every other type.)
  *
  * @tparam T The type of the contained elements.
  */
sealed abstract class Tree[+T] {
  /** P55 (**) Construct completely balanced binary trees.
    *
    * In a completely balanced binary tree, the following property holds for every node:
    * The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
    * which means their difference is not greater than one.
    * Define an object named Tree.
    * Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes.
    * The function should generate all solutions.
    * The function should take as parameters the number of nodes and a single value to put in all of them.
    *
    * Example: {{{
    *   Tree.cBalanced(4, "x")
    *   // res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
    * }}}
    *
    * @param n The number of nodes in the tree.
    * @param v The value to use for each node in the tree.
    * @tparam U The type of the value stored in the tree.
    * @return A list of all completely balanced binary trees with n nodes.
    */
  def cBalanced[U >: T]: (Int, U) => List[Tree[U]] = {
    case (n, _) if n <= 0 => List(End)
    case (n, v) if n == 0 => List(Node(v))
    case (n, v) if n % 2 != 0 =>
      val children = cBalanced(n - 1 / 2, v)
      for { l <- children; r <- children } yield Node(v, l, r)
    case (n, v) =>
      val greater = n - 1 / 2
      val greaterChildren = cBalanced(greater, v)
      val lesserChildren = cBalanced(greater + 1, v)
      val leftGreater = for {
        l <- greaterChildren
        r <- lesserChildren
      } yield Node(v, l, r)
      val rightGreater = for {
        l <- lesserChildren
        r <- greaterChildren
      } yield Node(v, l, r)
      leftGreater ::: rightGreater
  }

  def isMirrorOf[U >: T](other: Tree[U]): Boolean = (this, other) match {
    case (End, End) => true
    case (Node(_, tL, tR), Node(_, oL, oR)) => tL.isMirrorOf(oR) && tR.isMirrorOf(oL)
    case (_, _) => false
  }

  /** P56 (**) Symmetric binary trees.
    *
    * Let us call a binary tree symmetric if you can draw a vertical line through the root node
    * and then the right subtree is the mirror image of the left subtree.
    * Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric.
    * Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another.
    * We are only interested in the structure, not in the contents of the nodes.
    *
    * @return A list of all symmetric binary trees with a given number of nodes.
    */
  def isSymmetric: Boolean = this match {
    case End => true
    case Node(_, l, r) => l.isMirrorOf(r)
  }

  /** P57 (**) Binary search trees (dictionaries).
    *
    * Write a function to add an element to a binary search tree.
    *
    * Example: {{{
    *   val res0 = End.addValue(2)
    *   // res0: Node[Int] = T(2 . .)
    *
    *   val res1 = res0.addValue(3)
    *   // res1: Node[Int] = T(2 . T(3 . .))
    *
    *   val res2 = res1.addValue(0)
    *   // res2: Node[Int] = T(2 T(0 . .) T(3 . .))
    * }}}
    *
    * Hint: The abstract definition of addValue in Tree should be {{{ def addValue[U >: T <% Ordered[U]](x: U): Tree[U] }}}.
    * The {{{ >: T }}} is because addValue’s parameters need to be contravariant in T.
    * (Conceptually, we’re adding nodes above existing nodes.
    * In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or any supertype.)
    * The {{{ <% Ordered[U] }}} allows us to use the < operator on the values in the tree.
    *
    * Use that function to construct a binary tree from a list of integers.
    *
    * Example: {{{
    *   Tree.fromList(List(3, 2, 5, 7, 1))
    *   // res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
    * }}}
    *
    * Finally, use that function to test your solution to P56.
    *
    * Example: {{{
    *   Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
    *   // res4: Boolean = true
    *   Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
    *   // res5: Boolean = false
    * }}}
    *
    * @param U   The type of the value stored in the tree.
    * @param ord An implicit ordering on the values stored in the tree.
    */
  def addValue[U >: T](value: U)(implicit ord: Ordering[U]): Tree[U] = (this, value) match {
    case (End, _) => Node(value)
    case (Node(tV, l, r), v) if v <= tV => Node(tV, l.addValue(v), r)
    case (Node(tV, l, r), v) => Node(tV, l, r.addValue(v))
  }

  def fromList[U >: T](ls: List[U])(implicit ord: Ordering[U]): Tree[U] =
    ls.foldLeft[Tree[U]](End)((acc, curr) => acc.addValue(curr))

  /** P58 (**) Generate-and-test paradigm.
    *
    * @param n The number of nodes in the tree.
    * @param v The value to use for each node in the tree.
    * @tparam T The type of the value stored in the tree.
    * @return A list of all symmetric and completely balanced binary trees with n nodes.
    */
  def symmetricBalancedTrees2[T](n: Int, v: T): List[Tree[T]] = ???

  /** P59 (**) Construct height-balanced binary trees.
    *
    * @param height The desired height of the tree.
    * @param value  The value to use for each node in the tree.
    * @tparam T The type of the value stored in the tree.
    * @return A list of all height-balanced binary trees with the given height.
    */
  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = ???

  /** P60 (**) Construct height-balanced binary trees with a given number of nodes.
    *
    * @param n The number of nodes in the tree.
    * @param v The value to use for each node in the tree.
    * @tparam T The type of the value stored in the tree.
    * @return A list of all height-balanced binary trees with n nodes.
    */
  def hbalTreesWithNodes[T](n: Int, v: T): List[Tree[T]] = ???

  /** P61 (*) Count the leaves of a binary tree.
    *
    * @return The number of leaves in the tree.
    */
  def leafCount: Int = ???

  /** P61A (*) Collect the leaves of a binary tree in a list.
    *
    * @return A list of all leaves in the tree.
    */
  def leafList: List[T] = ???

  /** P62 (*) Collect the internal nodes of a binary tree in a list.
    *
    * An internal node of a binary tree has either one or two non-empty successors. Write a method internalList to
    * collect them in a list.
    *
    * @return A list of all internal nodes in the tree.
    */
  def internalList: List[T] = ???

  /** P62B (*) Collect the nodes at a given level in a list.
    * A node of a binary tree is at level N if the path from the root to the node has length N−1.
    * The root node is at level 1. Write a method atLevel to collect all nodes at a given level in a list.
    *
    * @param n The level of at which to collect the elements.
    * @return List of elements at level N.
    */
  def atLevel(n: Int): List[T] = ???
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

case object End extends Tree[Nothing] {
  override def toString: String = "."
}
