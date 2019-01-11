package app

import domain.RandomAlg
import domain.maths.NonNegativeNumber

import scala.util.Random

class AppRandom extends RandomAlg {

  override def randomDouble: Double = Random.nextDouble()

  override def randomNonNegativeDouble(max: NonNegativeNumber): Double = Random.nextDouble() * max.value
}
