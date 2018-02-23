package hu.bme.mit.inf.breeze.example

import breeze.linalg._
import org.scalatest.{FlatSpec, Matchers}

class SimpleSolverSpec extends FlatSpec with Matchers {
  "Our simple solver" should "give the steady state distribution of a 3-state ergodic CTMC" in {
    val q = DenseMatrix(
      (-4.0, 2.0, 2.0),
      (1.0, -2.0, 1.0),
      (1.0, 1.0, -2.0)
    )
    val Right(solution) = SimpleSolver.solveSteadyState(q)
    val expected = DenseVector(1.0, 2.0, 2.0) / 5.0


    norm(solution - expected) shouldBe <= (1e-10)
  }
}
