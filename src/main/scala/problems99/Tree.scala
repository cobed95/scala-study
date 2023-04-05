package org.laplacetec.study
package problems99

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def isSymmetric: Boolean = Tree.isMirrored(left, right)
}

case object End extends Tree[Nothing] {
  def isSymmetric: Boolean = true
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
}
