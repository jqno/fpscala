package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}
import Chapter3Tree._

class Chapter3TreeTest extends FlatSpec with Matchers {
  val someTree = Branch(
                   Branch(
                     Leaf(5),
                     Branch(
                       Leaf(3),
                       Leaf(8))),
                   Branch(
                     Leaf(1),
                     Leaf(2)))


  behavior of "size"

  it should "count the number of nodes and branches in a tree" in {
    treeSize(someTree) should be (9)
  }


  behavior of "maximum"

  it should "find the maximum value in the tree" in {
    maximum(someTree) should be (8)
  }
}
