package domain

import domain.maths.NonNegativeNumber

trait RandomNumber {

  def randomDouble: Double

  def randomNonNegativeDouble(max: NonNegativeNumber): Double
}
