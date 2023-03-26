package org.laplacetec.study
package problems99

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
  /** P54 (*) Check whether a given tree is symmetric.
    *
    * @return true if the tree is symmetric, false otherwise.
    */
  def isSymmetric: Boolean = ???

  /** P55 (**) Construct completely balanced binary trees.
    *
    * @param n The number of nodes in the tree.
    * @param v The value to use for each node in the tree.
    * @tparam T The type of the value stored in the tree.
    * @return A list of all completely balanced binary trees with n nodes.
    */
  def completeBinaryTree[T](n: Int, v: T): List[Tree[T]] = ???

  /** P56 (**) Symmetric binary trees.
    *
    * @return A list of all symmetric binary trees with a given number of nodes.
    */
  def symmetricBalancedTrees[T](n: Int, v: T): List[Tree[T]] = ???

  /** P57 (**) Binary search trees (dictionaries).
    *
    * @param T   The type of the value stored in the tree.
    * @param ord An implicit ordering on the values stored in the tree.
    */
  def addValue[T](value: T)(implicit ord: Ordering[T]): Tree[T] = ???

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
