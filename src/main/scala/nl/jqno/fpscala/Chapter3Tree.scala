package nl.jqno.fpscala

object Chapter3Tree {
  // 3.25: size
  def treeSize[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(a, b) => 1 + treeSize(a) + treeSize(b)
  }


  // 3.26: maximum
  def maximum(tree: Tree[Int]): Int = {
    def go(acc: Int, subtree: Tree[Int]): Int = subtree match {
      case Leaf(x) => acc max x
      case Branch(a, b) => acc max maximum(a) max maximum(b)
    }
    go(-1, tree)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
