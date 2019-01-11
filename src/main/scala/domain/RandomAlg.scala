package domain

import domain.maths.NonNegativeNumber

trait RandomAlg {

  def randomDouble: Double

  def randomNonNegativeDouble(max: NonNegativeNumber): Double
}
