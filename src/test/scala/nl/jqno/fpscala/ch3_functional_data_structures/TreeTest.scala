package nl.jqno.fpscala.ch3_functional_data_structures

import TreeFunctions._
import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {
  val someTree = Branch(
                   Branch(
                     Leaf(5),
                     Branch(
                       Leaf(3),
                       Leaf(8))),
                   Branch(
                     Leaf(1),
                     Leaf(2)))
  val plusOned = Branch(Branch(Leaf(6), Branch(Leaf(4), Leaf(9))), Branch(Leaf(2), Leaf(3)))
  val toStringed = Branch(Branch(Leaf("5"), Branch(Leaf("3"), Leaf("8"))), Branch(Leaf("1"), Leaf("2")))


  behavior of "size"

  it should "count the number of nodes and branches in a tree" in {
    treeSize(someTree) should be (9)
  }


  behavior of "maximum"

  it should "find the maximum value in the tree" in {
    maximum(someTree) should be (8)
  }


  behavior of "depth"

  it should "find the depth of a tree" in {
    depth(someTree) should be (4)
  }


  behavior of "map"

  it should "apply a function to each element of the tree" in {
    map(someTree)(_ + 1) should be (plusOned)
    map(someTree)(_.toString) should be (toStringed)
  }


  behavior of "fold"

  it should "implement size" in {
    fold(someTree)(_ => 1)(1 + _ + _) should be (9)
  }

  it should "implement maximum" in {
    fold(someTree)(identity)(_ max _) should be (8)
  }

  it should "implement depth" in {
    fold(someTree)(_ => 1)((a, b) => 1 + (a max b)) should be (4)
  }

  it should "also do mappy things" in {
    def myMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

    myMap(someTree)(_ + 1) should be (plusOned)
    myMap(someTree)(_.toString) should be (toStringed)
  }
}
