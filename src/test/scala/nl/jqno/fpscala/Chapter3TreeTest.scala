package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}
import Chapter3Tree._

class Chapter3TreeTest extends FlatSpec with Matchers {
  val someTree = Branch(
                   Branch(
                     Leaf("a"),
                     Branch(
                       Leaf("b"),
                       Leaf("c"))),
                   Branch(
                     Leaf("d"),
                     Leaf("e")))


  behavior of "size"

  it should "count the number of nodes and branches in a tree" in {
    treeSize(someTree) should be (9)
  }
}
