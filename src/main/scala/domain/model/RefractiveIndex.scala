package domain.model

import domain.error.InvalidArgumentError

sealed abstract case class RefractiveIndex(value: Double)

object RefractiveIndex {

  val ofVacuum: RefractiveIndex = new RefractiveIndex(1) {}

  def apply(value: Double): Either[InvalidArgumentError, RefractiveIndex] =
    if (value >= 1) {
      Right( new RefractiveIndex(value) {} )
    } else {
      Left(InvalidArgumentError(s"Cannot have a refractive index of $value, it must be at least 1"))
    }
}
