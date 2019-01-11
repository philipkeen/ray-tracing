package test

import domain.RandomAlg
import domain.maths.NonNegativeNumber

class TestRandom extends RandomAlg {
  override def randomDouble: Double = 1

  override def randomNonNegativeDouble(max: NonNegativeNumber): Double = 1
}
