package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}

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

}
