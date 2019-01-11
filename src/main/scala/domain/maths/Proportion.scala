package domain.maths

import domain.utils.InvalidArgumentError

sealed abstract case class Proportion(value: Double) {

  def *(that: Proportion): Proportion = new Proportion(value * that.value) {}
}

object Proportion {
  def apply(value: Double): Either[InvalidArgumentError, Proportion] =
    if (value >= 0 && value <= 1) {
      Right(new Proportion(value) {})
    } else {
      Left(InvalidArgumentError(s"Unable to create Proportion: $value lies outside [0, 1] interval"))
    }

  val Zero: Proportion = new Proportion(0) {}

  val One: Proportion = new Proportion(1) {}
}
