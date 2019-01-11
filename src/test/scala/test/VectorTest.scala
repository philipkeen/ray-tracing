package test

import domain.maths.CartesianVector

trait VectorTest {

  private val Epsilon = math.pow(10, -12)

  def negligibleDifference(vector1: CartesianVector, vector2: CartesianVector): Boolean =
    (vector1 distanceTo vector2) < Epsilon

}
