package org.laplacetec.study
package problems99

import scala.math.Ordering.Implicits.infixOrderingOps
import javax.swing.text.Position

/** A binary tree is either empty or it is composed of a root element and two
  * successors, which are binary trees themselves. We shall use the following
  * classes to represent binary trees. (Also available in tree1.scala.) An End
  * is equivalent to an empty tree. A Branch has a value, and two descendant
  * trees. The toString functions are relatively arbitrary, but they yield a
  * more compact output than Scala's default. Putting a plus in front of the T
  * makes the class covariant; it will be able to hold subtypes of whatever type
  * it's created for. (This is important so that End can be a singleton object;
  * as a singleton, it must have a specific type, so we give it type Nothing,
  * which is a subtype of every other type.)
  *
  * @tparam T
  *   The type of the contained elements.
  */
sealed abstract class Tree[+T] {

  /** P55 (**) Construct completely balanced binary trees.
    *
    * In a completely balanced binary tree, the following property holds for
    * every node: The number of nodes in its left subtree and the number of
    * nodes in its right subtree are almost equal, which means their difference
    * is not greater than one. Define an object named Tree. Write a function
    * Tree.cBalanced to construct completely balanced binary trees for a given
    * number of nodes. The function should generate all solutions. The function
    * should take as parameters the number of nodes and a single value to put in
    * all of them.
    *
    * Example: {{{ Tree.cBalanced(4, "x") // res0: List(Node[String]) = List(T(x
    * T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ... }}}
    *
    * @param n
    *   The number of nodes in the tree.
    * @param v
    *   The value to use for each node in the tree.
    * @tparam U
    *   The type of the value stored in the tree.
    * @return
    *   A list of all completely balanced binary trees with n nodes.
    */
  def cBalanced[U >: T](n: Int, v: U): List[Tree[U]] = (n, v) match {
    case (n, v) if n < 1 => List(End)
    case (n, v) if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, v)
      subtrees.flatMap(l => subtrees.map(r => Node(v, l, r)))
    }
    case (n, v) => {
      val smaller = cBalanced(n / 2, v)
      val bigger = cBalanced(n / 2 - 1, v)
      smaller.flatMap(l =>
        bigger.flatMap(r => List(Node(v, l, r), Node(v, r, l)))
      )
    }
  }

  /** P56 (**) Symmetric binary trees.
    *
    * Let us call a binary tree symmetric if you can draw a vertical line
    * through the root node and then the right subtree is the mirror image of
    * the left subtree. Add an isSymmetric method to the Tree class to check
    * whether a given binary tree is symmetric. Hint: Write an isMirrorOf method
    * first to check whether one tree is the mirror image of another. We are
    * only interested in the structure, not in the contents of the nodes.
    *
    * @return
    *   A list of all symmetric binary trees with a given number of nodes.
    */
  def isSymmetric: Boolean = this match {
    case Node(v, l, r) => l.isMirrorOf(r)
    case End           => true
  }

  def isMirrorOf[U >: T](right: Tree[U]): Boolean = (this, right) match {
    case (End, End)           => true
    case (End, Node(_, _, _)) => false
    case (Node(_, _, _), End) => false
  }

  /** P57 (**) Binary search trees (dictionaries).
    *
    * Write a function to add an element to a binary search tree.
    *
    * Example: {{{ val res0 = End.addValue(2) // res0: Node[Int] = T(2 . .)
    *
    * val res1 = res0.addValue(3) // res1: Node[Int] = T(2 . T(3 . .))
    *
    * val res2 = res1.addValue(0) // res2: Node[Int] = T(2 T(0 . .) T(3 . .))
    * }}}
    *
    * Hint: The abstract definition of addValue in Tree should be
    * {{{def addValue[U >: T <% Ordered[U]](x: U): Tree[U]}}}. The {{{>: T}}} is
    * because addValue's parameters need to be contravariant in T.
    * (Conceptually, we're adding nodes above existing nodes. In order for the
    * subnodes to be of type T or any subtype, the upper nodes must be of type T
    * or any supertype.) The {{{<% Ordered[U]}}} allows us to use the < operator
    * on the values in the tree. Use that function to construct a binary tree
    * from a list of integers.
    *
    * Example: {{{ Tree.fromList(List(3, 2, 5, 7, 1)) // res3: Node[Int] = T(3
    * T(2 T(1 . .) .) T(5 . T(7 . .))) }}}
    *
    * Finally, use that function to test your solution to P56.
    *
    * Example: {{{ Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric //
    * res4: Boolean = true Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric //
    * res5: Boolean = false }}}
    * @param U
    *   The type of the value stored in the tree.
    * @param ord
    *   An implicit ordering on the values stored in the tree.
    */
  def addValue[U >: T](value: U)(implicit ord: Ordering[U]): Tree[U] = ???

  def fromList[U >: T](ls: List[U])(implicit ord: Ordering[U]): Tree[U] = ???

  /** P58 (**) Generate-and-test paradigm.
    *
    * Apply the generate-and-test paradigm to construct all symmetric,
    * completely balanced binary trees with a given number of nodes.
    *
    * @param n
    *   The number of nodes in the tree.
    * @param v
    *   The value to use for each node in the tree.
    * @tparam U
    *   The type of the value stored in the tree.
    * @return
    *   A list of all symmetric and completely balanced binary trees with n
    *   nodes.
    */
  1
  def symmetricBalancedTrees[U >: T](n: Int, v: U): List[Tree[U]] =
    cBalanced(n, v).filter(_.isSymmetric)

  /** P59 (**) Construct height-balanced binary trees.
    *
    * In a height-balanced binary tree, the following property holds for every
    * node: The height of its left subtree and the height of its right subtree
    * are almost equal, which means their difference is not greater than one.
    *
    * Write a method Tree.hbalTrees to construct height-balanced binary trees
    * for a given height with a supplied value for the nodes. The function
    * should generate all solutions.
    *
    * Example: {{{ Tree.hbalTrees(3, "x") // res0: List[Node[String]] = List(T(x
    * T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .))
    * T(x T(x . .) .)), ...  }}}
    *
    * @param height
    *   The desired height of the tree.
    * @param value
    *   The value to use for each node in the tree.
    * @tparam U
    *   The type of the value stored in the tree.
    * @return
    *   A list of all height-balanced binary trees with the given height.
    */
  def hbalTrees[U >: T](height: Int, value: U): List[Tree[U]] =
    (height, value) match {
      case (h, v) if h <= 0 => List(End)
      case (h, v) if h == 1 => List(Node(v, End, End))
      case (h, v) => {
        val largeTrees = hbalTrees(h - 1, v)
        val smallTrees = hbalTrees(h - 2, v)

        largeTrees.flatMap(l =>
          smallTrees.flatMap(s =>
            List(Node(v, l, s), Node(v, s, l), Node(v, l, l))
          )
        )
      }
    }

  /** P60 (**) Construct height-balanced binary trees with a given number of
    * nodes.
    *
    * Consider a height-balanced binary tree of height H. What is the maximum
    * number of nodes it can contain? Clearly, N_max = 2&#94;H - 1. However,
    * what is the minimum number N_min? This question is more difficult. Try to
    * find a recursive statement and turn it into a function minHbalNodes that
    * takes a height and returns N_min.
    *
    * {{{
    *   minHbalNodes(3)
    *   // res0: Int = 4
    * }}}
    *
    * On the other hand, we might ask: hwat is the maximum height H a
    * height-balanced binary tree with N nodes can have? Write a maxHbalHeight
    * function.
    *
    * {{{
    *   maxHbalHeight(4)
    *   // res1: Int = 3
    * }}}
    *
    * Now, we can attack the main problem: construct all the height-balanced
    * binary trees with a given number of nodes.
    *
    * {{{
    *   Tree.hbalTreesWithNodes(4, "x")
    *   // res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
    * }}}
    *
    * Find out how many height-balanced trees exist for N = 15.
    *
    * @param h
    *   The height of the tree.
    * @return
    *   The minimum number of nodes N_min for a height-balanced tree.
    */
  def minHbalNodes(h: Int): Int = h match {
    case h if h <= 0 => 0
    case h if h == 1 => 1
    case h           => 1 + minHbalNodes(h - 1) + minHbalNodes(h - 2)
  }

  /** Maximum height of a height-balanced tree for a given number of nodes
    *
    * @param n
    *   The number of nodes.
    * @return
    *   The maximum height H of height-balanced trees.
    */
  def maxHbalHeight(n: Int): Int = {
    def helper(height: Int): Int = {
      if (n < minHbalNodes(height)) height - 1
      else helper(height + 1)
    }
    helper(0)
  }

  /** All height balanced trees given the number of nodes.
    *
    * @param n
    *   The number of nodes in the tree.
    * @param v
    *   The value to use for each node in the tree.
    * @tparam U
    *   The type of the value stored in the tree.
    * @return
    *   A list of all height-balanced binary trees with n nodes.
    */
  def minHeight(n: Int): Int = {
    if (n == 0) 0
    else minHeight(n / 2) + 1
  }
  def nodeCount: Int = this match {
    case End           => 0
    case Node(v, l, r) => 1 + l.nodeCount + r.nodeCount
  }
  def hbalTreesWithNodes[U >: T](n: Int, v: U): List[Tree[U]] = {
    (minHeight(n) to maxHbalHeight(n))
      .flatMap(x => hbalTrees(x, v))
      .toList
      .filter(_.nodeCount == n)
  }

  /** P61 (*) Count the leaves of a binary tree.
    *
    * A leaf is a node with no successors. Write a method leafCount to count
    * them.
    *
    * {{{
    *   Node('x', Node('x'), End).leafCount
    *   // res0: Int = 1
    * }}}
    *
    * @return
    *   The number of leaves in the tree.
    */
  def leafCount: Int = this match {
    case End               => 0
    case Node(_, End, End) => 1
    case Node(_, l, r)     => l.leafCount + r.leafCount
  }

  /** P61A (*) Collect the leaves of a binary tree in a list.
    *
    * A leaf is a node with no successors. Write a method leafList to collect
    * them in a list.
    *
    * {{{
    *   Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
    *   // res0: List[Char] = List(b, d, e)
    * }}}
    *
    * @return
    *   A list of all leaves in the tree.
    */
  def leafList[U >: T]: List[U] = ???

  /** P62 (*) Collect the internal nodes of a binary tree in a list.
    *
    * An internal node of a binary tree has either one or two non-empty
    * successors. Write a method internalList to collect them in a list.
    *
    * {{{
    *   Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
    *   // res0: List[Char] = List(a, c)
    * }}}
    *
    * @return
    *   A list of all internal nodes in the tree.
    */
  def internalList[U >: T]: List[U] = ???

  /** P62B (*) Collect the nodes at a given level in a list. A node of a binary
    * tree is at level N if the path from the root to the node has length N−1.
    * The root node is at level 1. Write a method atLevel to collect all nodes
    * at a given level in a list.
    *
    * {{{
    *   Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
    *   // res0: List[Char] = List(b, c)
    * }}}
    *
    * @param n
    *   The level of at which to collect the elements.
    * @return
    *   List of elements at level N.
    */
  def atLevel[U >: T](n: Int): List[U] = ???

  /** P63 (**) Construct a complete binary tree.
    *
    * A complete binary tree with height H is defined as follows: The levels
    * 1,2,3,…,H−1 contain the maximum number of nodes (i.e 2&#94;i−1 at the
    * level i, note that we start counting the levels from 1 at the root). In
    * level H, which may contain less than the maximum possible number of nodes,
    * all the nodes are “left-adjusted”. This means that in a levelorder tree
    * traversal all internal nodes come first, the leaves come second, and empty
    * successors (the Ends which are not really nodes!) come last.
    *
    * Particularly, complete binary trees are used as data structures (or
    * addressing schemes) for heaps.
    *
    * We can assign an address number to each node in a complete binary tree by
    * enumerating the nodes in levelorder, starting at the root with number 1.
    * In doing so, we realize that for every node X with address A the following
    * property holds: The address of X’s left and right successors are 2 * A and
    * 2 * A + 1, respectively, supposed the successors do exist. This fact can
    * be used to elegantly construct a complete binary tree structure. Write a
    * method completeBinaryTree that takes as parameters the number of nodes and
    * the value to put in each node.
    *
    * {{{
    *   Tree.completeBinaryTree(6, "x")
    *   // res0: Node[String] = T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))
    * }}}
    *
    * @param n
    *   The number of nodes
    * @param v
    *   The value to put inside each node
    * @tparam U
    *   The type of the value in each node
    * @return
    *   The complete binary tree with n nodes.
    */
  def completeBinaryTree[U >: T](n: Int, v: U): Tree[U] = ???

  def layoutXYPosition[U >: T](
      node: Node[U],
      order: Int,
      height: Int
  ): (PositionedNode[U], Int) = node match {
    case Node(v, End, End) =>
      (PositionedNode(v, End, End, order + 1, height), order + 1)
    case Node(v, l: Node[U], End) => {
      val (left, leftOrder) = layoutXYPosition(l, order, height + 1)
      val curOrder = leftOrder + 1
      (
        PositionedNode(v, left, End, curOrder, height),
        curOrder
      )
    }
    case Node(v, End, r: Node[U]) => {
      val curOrder = order + 1
      val (right, rightOrder) = layoutXYPosition(r, curOrder, height + 1)
      (
        PositionedNode(v, End, right, curOrder, height),
        rightOrder
      )
    }
    case Node(v, l: Node[U], r: Node[U]) => {
      val (left, leftOrder) = layoutXYPosition(l, order, height + 1)
      val curOrder = leftOrder + 1
      val (right, rightOrder) = layoutXYPosition(r, curOrder, height + 1)
      (
        PositionedNode(v, left, right, curOrder, height),
        rightOrder
      )
    }
    case Node(_, _, _) => throw new Exception("Invalid tree")
  }

  def layoutBinaryTree[U >: T]: Tree[U] = this match {
    case End                  => End
    case n: PositionedNode[U] => n
    case n: Node[U]           => layoutXYPosition(n, 0, 1)._1
  }

  def layoutXYPosition2[U >: T](
      maxHeight: Int,
      curHeight: Int,
      xPosition: Int
  ): PositionedNode[U] = this match {
    case Node(v, End, End) => PositionedNode(v, End, End, xPosition, curHeight)
    case Node(v, l: Node[U], End) => {
      val delta = math.pow(2, maxHeight - curHeight - 1).toInt
      val left = l.layoutXYPosition2(
        maxHeight,
        curHeight + 1,
        xPosition - delta
      )
      PositionedNode(v, left, End, xPosition, curHeight)
    }
    case Node(v, End, r: Node[U]) => {
      val delta = math.pow(2, maxHeight - curHeight - 1).toInt
      val right = r.layoutXYPosition2(
        maxHeight,
        curHeight + 1,
        xPosition + delta
      )
      PositionedNode(v, End, right, xPosition, curHeight)
    }
    case Node(v, l: Node[U], r: Node[U]) => {
      val delta = math.pow(2, maxHeight - curHeight - 1).toInt
      val left = l.layoutXYPosition2(
        maxHeight,
        curHeight + 1,
        xPosition - delta
      )
      val right = r.layoutXYPosition2(
        maxHeight,
        curHeight + 1,
        xPosition + delta
      )
      PositionedNode(v, left, right, xPosition, curHeight)
    }

    case _ => throw new Exception("Invalid tree")
  }
  def getRootXPosition(
      maxHeight: Int,
      width: Int,
      height: Int
  ): Int =
    this match {
      case Node(_, End, _) => width
      case Node(_, left: Node[_], _) => {
        val leftXPosition = left.getRootXPosition(maxHeight, width, height + 1)
        leftXPosition + math.pow(2, maxHeight - height - 1).toInt
      }
      case _ => throw new Exception("Invalid tree")
    }

  def getMaxHeight(curHeight: Int): Int = this match {
    case Node(_, End, End) => curHeight
    case Node(_, left: Node[_], End) =>
      left.getMaxHeight(curHeight + 1)
    case Node(_, End, right: Node[_]) =>
      right.getMaxHeight(curHeight + 1)
    case Node(_, left: Node[_], right: Node[_]) =>
      math.max(
        left.getMaxHeight(curHeight + 1),
        right.getMaxHeight(curHeight + 1)
      )
    case _ => throw new Exception("Invalid tree")
  }

  def layoutBinaryTree2[U >: T]: Tree[U] = this match {
    case End                  => End
    case n: PositionedNode[U] => n
    case n: Node[U] => {
      val maxHeight = n.getMaxHeight(1)
      val rootXPosition = n.getRootXPosition(maxHeight, 1, 1)
      layoutXYPosition2(maxHeight, 1, rootXPosition)
    }
  }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String =
    "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  /** P65 (**) Layout a binary tree (2).
    *
    * An alternative layout method is depicted in the illustration opposite.
    * Find out the rules and write the corresponding method. Hint: On a given
    * level, the horizontal distance between neighboring nodes is constant.
    *
    * (See [[https://aperiodic.net/phil/scala/s-99/#p65 P65 problem spec]] for
    * image.)
    *
    * Use the same conventions as in problem P64.
    *
    * {{{
    *   Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2
    *   // res0: PositionedNode[Char] = T[3,1]('a T[1,2]('b . T[2,3]('c . .)) T[5,2]('d . .))
    * }}}
    *
    * The tree at right may be constructed with
    * {{{Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q'))}}}.
    * Use it to check your code.
    *
    * @tparam U
    *   The type of the trees contents.
    * @return
    *   Tree of PositionedNodes with x_v and y_v
    */

  /** P66 (***) Layout a binary tree (3).
    *
    * Yet another layout strategy is shown in the illustration opposite. The
    * method yields a very compact layout while maintaining a certain symmetry
    * in every node. Find out the rules and write the corresponding method.
    * Hint: Consider the horizontal distance between a node and its successor
    * nodes. How tight can you pack together two subtrees to construct the
    * combined binary tree?
    *
    * (See [[https://aperiodic.net/phil/scala/s-99/#p66 P66 problem spec]] for
    * image.)
    *
    * Use the same conventions as in problem P64 and P65. Note: This is a
    * difficult problem. Don’t give up too early!
    *
    * {{{
    *   Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree3
    *   // res0: PositionedNode[Char] = T[2,1]('a T[1,2]('b . T[2,3]('c . .)) T[3,2]('d . .))
    * }}}
    *
    * @tparam U
    *   The type of the trees contents.
    * @return
    *   Tree of PositionedNodes with x_v and y_v
    */
  def layoutBinaryTree3[U >: T]: PositionedNode[U] = ???
}

case class PositionedNode[+T](
    value: T,
    left: Tree[T],
    right: Tree[T],
    x: Int,
    y: Int
) extends Tree[T] {
  override def toString =
    "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

case object End extends Tree[Nothing] {
  override def toString: String = "."
}

trait NodeTrait[+T] {
  def value: T
  def left: Tree[T]
  def right: Tree[T]
}
