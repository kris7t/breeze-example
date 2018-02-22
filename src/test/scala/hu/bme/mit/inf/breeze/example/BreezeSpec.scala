package hu.bme.mit.inf.breeze.example

import breeze.linalg._
import org.scalatest.{FlatSpec, Matchers}

class BreezeSpec extends FlatSpec with Matchers {

  val matrix = DenseMatrix(
    (1.0, 2.0, 3.0),
    (4.0, 5.0, 6.0),
    (7.0, 8.0, 9.0)
  )

  val v = DenseVector(1.0, -1.0, 0.0)

  "Dense matrix multiplication" should "multiply a matrix with a vector" in {
    val b = matrix * v

    b should equal (DenseVector(-1.0, -1.0, -1.0))
    b(0) should equal (-1.0)
    matrix(1, 2) should equal (6.0)
  }

  it should "multiply a vector with a matrix" in {
    val b = (v.t * matrix).t

    b should equal (DenseVector(-3.0, -3.0, -3.0))
    true shouldBe true
  }

  it should "solve every problem on Earth" in pending
}
