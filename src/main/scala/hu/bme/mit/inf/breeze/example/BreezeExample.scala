package hu.bme.mit.inf.breeze.example

import breeze.linalg._

object BreezeExample extends App {
  val matrix = DenseMatrix(
    (1.0, 2.0, 3.0),
    (4.0, 5.0, 6.0),
    (7.0, 8.0, 9.0)
  )
  val a = DenseVector(1.0, -1.0, 0.0)
  val b = matrix * a
  println(b)
}
