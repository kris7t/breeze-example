package hu.bme.mit.inf.breeze.example

import breeze.linalg._

sealed trait Solution
case class ValidSolution(p: DenseVector[Double]) extends Solution
case object NotAMarkovChain extends Solution
case object ReducibleMarkovChain extends Solution

trait Solver {
  def solveSteadyState(q: DenseMatrix[Double]): Solution
}

object Solver {
  def solutionToMessage(solution: Solution): String = solution match {
    case ValidSolution(p) => s"Got a solution: $p"
    case NotAMarkovChain => "Not a Markov chain"
    case ReducibleMarkovChain => "Solution is not unique"
  }
}

case class SimpleSolver(threshold: Double) extends Solver {
  require(threshold > 0, "threshold must be positive")

  override def solveSteadyState(q: DenseMatrix[Double]): Solution = {
    val eig.Eig(re, im, v) = eig(q.t)
    for (i <- 0 until re.length) {
      if (re(i) * re(i) + im(i) * im(i) <= threshold) {
        val solution = v(::, 1)
        return ValidSolution(solution / sum(solution))
      }
    }
    NotAMarkovChain
  }
}

object SimpleSolver extends SimpleSolver(1e-10)
