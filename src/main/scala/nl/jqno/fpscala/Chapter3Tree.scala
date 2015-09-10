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


  // 3.27: depth
  def depth[A](tree: Tree[A]): Int = {
    def go(acc: Int, subtree: Tree[A]): Int = subtree match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + (depth(a) max depth(b))
    }
    go(0, tree)
  }


  // 3.28: map
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(a, b) => Branch(map(a)(f), map(b)(f))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
