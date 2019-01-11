package app

import domain.RandomNumber
import domain.maths.NonNegativeNumber

import scala.util.Random

class AppRandom extends RandomNumber {

  override def randomDouble: Double = Random.nextDouble()

  override def randomNonNegativeDouble(max: NonNegativeNumber): Double = Random.nextDouble() * max.value
}
