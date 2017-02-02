import Par._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures
import java.util.concurrent.Executors

class ParTest extends FlatSpec with Matchers with ScalaFutures {
  val pool = Executors.newFixedThreadPool(2)

  // 7.5: sequence
  behavior of "sequence"

  it should "return the sequence in the right order" in {
    val in = List(unit(1), unit(2), unit(3))
    val out = sequence(in)
    val actual = Par.run(pool)(out).get
    actual should be (List(1, 2, 3))
  }


  // 7.6: parFilter
  behavior of "parFilter"

  it should "return a Par of a filtered sequence" in {
    val in = List(1, 2, 3, 4)
    val out = parFilter(in)(_ % 2 == 0)
    val actual = Par.run(pool)(out).get
    actual should be (List(2, 4))
  }
}
