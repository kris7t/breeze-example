package hu.bme.mit.inf.breeze.example

import breeze.linalg._

sealed trait SolverError
case object NotAMarkovChain extends SolverError
case class ReducibleMarkovChain(kernelDimension: Int) extends SolverError

trait Solver {
  def solveSteadyState(q: DenseMatrix[Double]): Solver.Solution
}

object Solver {
  type Solution = Either[SolverError, DenseVector[Double]]

  def solutionToMessage(solution: Solution): String = solution match {
    case Right(p) => s"Got a solution: $p"
    case Left(NotAMarkovChain) => "Not a Markov chain"
    case Left(ReducibleMarkovChain(d)) => s"Solution is not unique, there are $d solutions"
  }
}

class SimpleSolver(threshold: Double) extends Solver {
  require(threshold > 0, "threshold must be positive")

  override def solveSteadyState(q: DenseMatrix[Double]): Solver.Solution = {
    val eig.Eig(re, im, v) = eig(q.t)
    0 until v.cols filter { i => re(i) * re(i) + im(i) * im(i) <= threshold} match {
      case Seq() => Left(NotAMarkovChain)
      case Seq(i) =>
        val solution = v(::, i)
        Right(solution / sum(solution))
      case indexes => Left(ReducibleMarkovChain(indexes.size))
    }
  }
}

object SimpleSolver extends SimpleSolver(1e-10) {
  def withThreshold(threshold: Double): SimpleSolver = new SimpleSolver(threshold)
}
