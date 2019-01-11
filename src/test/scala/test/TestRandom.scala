package test

import domain.RandomNumber
import domain.maths.NonNegativeNumber

class TestRandom extends RandomNumber {
  override def randomDouble: Double = 1

  override def randomNonNegativeDouble(max: NonNegativeNumber): Double = 1
}
