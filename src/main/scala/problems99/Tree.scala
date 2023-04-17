package org.laplacetec.study
package problems99

sealed abstract class Tree[+T] {
  def isSymmetric: Boolean

  def leafCount: Int

  def leafList: List[T]

  def internalList: List[T]

  def atLevel: Int => List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def isSymmetric: Boolean = Tree.isMirrored(left, right)

  def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case (_, _)     => left.leafCount + right.leafCount
  }

  def leafList: List[T] = (left, right) match {
    case (End, End) => List(value)
    case (_, _)     => left.leafList ::: right.leafList
  }

  def internalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case (_, _)     => value :: left.internalList ::: right.internalList
  }

  def atLevel: Int => List[T] = { x =>
    (left, right) match {
      case (End, End)       => Nil
      case (_, _) if x == 1 => List(value)
      case (_, _)           => left.atLevel(x - 1) ::: right.atLevel(x - 1)
    }
  }

  def layoutBinaryTree: Tree[T] = {
    def layoutBinaryTree: (Int, Int) => (Tree[T], Int) = { (x, y) =>
      val (leftPositionedNode, leftPositionedNodeHighestX) =
        layoutBinaryTree(x, y + 1)
      val (rightPositionedNode, rightPositionedNodeHighestX) =
        layoutBinaryTree(leftPositionedNodeHighestX + 2, y + 1)
      (
        PositionedNode(
          value,
          leftPositionedNode,
          rightPositionedNode,
          leftPositionedNodeHighestX + 1,
          y
        ),
        rightPositionedNodeHighestX
      )
    }
    layoutBinaryTree(1, 1)._1
  }
}

case object End extends Tree[Nothing] {
  def isSymmetric: Boolean = true

  def leafCount: Int = 0

  def leafList: List[Nothing] = Nil

  def internalList: List[Nothing] = Nil

  def atLevel: Int => List[Nothing] = Nil
}

object Tree {
  def cBalanced[T]: (Int, T) => List[Tree[T]] = {
    case (n, x) if n < 1 => List(End)
    case (n, x) if n % 2 == 1 => {
      val trees = cBalanced(n / 2, x)
      trees.flatMap({ left =>
        trees.map({ right =>
          Node(x, left, right)
        })
      })
    }
    case (n, x) => {
      val evenTrees = cBalanced((n - 1) / 2, x)
      val oddTrees = cBalanced((n - 1) / 2 + 1, x)
      evenTrees.flatMap({ evenTree =>
        oddTrees.flatMap({ oddTree =>
          List(Node(x, evenTree, oddTree), Node(x, oddTree, evenTree))
        })
      })
    }
  }

  def isMirrored[T]: (Tree[T], Tree[T]) => Boolean = {
    case (End, End) => true
    case (End, _)   => false
    case (_, End)   => false
    case (left: Node[T], right: Node[T]) =>
      isMirrored(left.left, right.right) && isMirrored(left.right, right.left)
  }

  def symmetricBalancedTrees[T]: (Int, T) => List[Tree[T]] = { (n, x) =>
    cBalanced(n, x).filter(tree => tree.isSymmetric)
  }

  def hbalTrees[T]: (Int, T) => List[Tree[T]] = {
    case (n, x) if n < 1 => List(End)
    case (1, x)          => List(Node(x, End, End))
    case (n, x) => {
      val oneSmallerTrees = hbalTrees(n - 1, x)
      val twoSmallerTrees = hbalTrees(n - 2, x)
      val sameHeightTrees = oneSmallerTrees.flatMap({ left =>
        oneSmallerTrees.map({ right =>
          Node(x, left, right)
        })
      })
      val differentHeightTrees = oneSmallerTrees.flatMap({ oneSmallerTree =>
        twoSmallerTrees.flatMap({ twoSmallerTree =>
          List(
            Node(x, oneSmallerTree, twoSmallerTree),
            Node(x, twoSmallerTree, oneSmallerTree)
          )
        })
      })
      sameHeightTrees ::: differentHeightTrees
    }
  }

  def completeBinaryTree[T]: (Int, T) => Tree[T] = { (x, y) =>
    def treeByAddress: Int => Tree[T] = {
      case z if z > x => End
      case z          => Node(y, treeByAddress(2 * z), treeByAddress(2 * z + 1))
    }
    treeByAddress(1)
  }
}

case class PositionedNode[+T](override val value: T,
                              override val left: Tree[T],
                              override val right: Tree[T],
                              x: Int,
                              y: Int)
    extends Node[T](value, left, right)
