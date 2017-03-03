package nl.jqno.fpscala.ch10_monoids

import org.scalatest.{FlatSpec, Matchers}
import nl.jqno.fpscala.ch3_functional_data_structures.{Tree, Leaf, Branch}
import nl.jqno.fpscala.ch7_parallelism.Par
import nl.jqno.fpscala.ch7_parallelism.Par._
import Monoids._

class MonoidsTest extends FlatSpec with Matchers {
  // Exercise 10.5: foldMap
  behavior of "foldMap"

  it should "map the values of a list and fold over the results" in {
    val as = List("1", "2", "3", "4")
    foldMap(as, intAddition)(_.toInt) should be (10)
  }


  // Exercise 10.6: foldRight in terms of foldMap
  behavior of "foldRight1"

  it should "fold over a list" in {
    val as = List("1", "2", "3", "4")
    foldRight1(as)(0)((a, b) => a.toInt + b) should be (10)
  }


  // Exercise 10.7: balanced foldMap
  behavior of "balanced foldMap"

  it should "map the values of a list of even size and fold over the results" in {
    val as = IndexedSeq("1", "2", "3", "4")
    foldMap(as, intAddition)(_.toInt) should be (10)
  }

  it should "map the values of a list of odd size and fold over the results" in {
    val as = IndexedSeq("1", "2", "3", "4", "5")
    foldMap(as, intAddition)(_.toInt) should be (15)
  }


  // Exercise 10.8: parFoldMap
  behavior of "parFoldMap"

  it should "map the values of a list and fold over the results" in {
    val as = IndexedSeq("1", "2", "3", "4")
    get(parFoldMap(as, intAddition)(_.toInt)) should be (10)
  }


  // Exercise 10.9: isSorted
  behavior of "isSorted"

  it should "return true if a list is sorted" in {
    val as = IndexedSeq(1, 2, 3, 4)
    isSorted(as) should be (true)
  }

  it should "return false if a list is unsorted" in {
    val as = IndexedSeq(1, 5, 3, 4)
    isSorted(as) should be (false)
  }


  // Exercise 10.11: wordcount
  behavior of "wordcount"

  it should "count the words in a string" in {
    wordcount("lorem ipsum dolor sit amet, ") should be (5)
  }


  // Exercise 10.15: Foldable.toList
  behavior of "Foldable.toList"

  it should "turn a List into the same List" in {
    val as = List(1, 2, 3, 4)
    FoldableList.toList(as) should be (as)
  }

  it should "turn a Tree into a List" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
    FoldableTree.toList(tree) should be (List(1, 2, 3, 4, 5))
  }

  it should "turn an Option into a List" in {
    FoldableOption.toList(Some(1)) should be (List(1))
    FoldableOption.toList(None) should be (Nil)
  }

  
  // Exercise 10.18: bag
  behavior of "bag"

  it should "act like a bag" in {
    val in = Vector("a", "rose", "is", "a", "rose")
    val expected = Map("a" -> 2, "rose" -> 2, "is" -> 1)
    bag(in) should be (expected)
  }


  private val pool = java.util.concurrent.Executors.newFixedThreadPool(2)
  private def get[A](p: Par[A]): A =
    Par.run(pool)(p).get
}

