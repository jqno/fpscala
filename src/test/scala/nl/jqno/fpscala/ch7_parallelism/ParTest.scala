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
    val actual = get(out)
    actual should be (List(1, 2, 3))
  }


  // 7.6: parFilter
  behavior of "parFilter"

  it should "return a Par of a filtered sequence" in {
    val in = List(1, 2, 3, 4)
    val out = parFilter(in)(_ % 2 == 0)
    val actual = get(out)
    actual should be (List(2, 4))
  }

  // 7.11: choiceN and choice
  behavior of "choiceN"

  it should "return the first element if n == 0" in {
    val choices = List(unit(0), unit(1), unit(2), unit(3))
    val out = choiceN(unit(0), choices)
    val actual = get(out)
    actual should be (0)
  }

  it should "return element #2 if n == 2" in {
    val choices = List(unit(0), unit(1), unit(2), unit(3))
    val out = choiceN(unit(2), choices)
    val actual = get(out)
    actual should be (2)
  }

  it should "I dunno, throw an exception if n is out of range of the choices I guess" in {
    val choices = Nil
    intercept[IllegalArgumentException] {
      get(choiceN(unit(0), choices))
    }
  }


  behavior of "choice"

  it should "return the first choice if cond evaluates to true" in {
    val out = choice(unit(true))(unit(0), unit(1))
    val actual = get(out)
    actual should be (0)
  }

  it should "return the second choice if cond evaluates to false" in {
    val out = choice(unit(false))(unit(0), unit(1))
    val actual = get(out)
    actual should be (1)
  }

  private def get[A](p: Par[A]): A =
    Par.run(pool)(p).get
}
