package domain.model

import domain.maths.{CartesianVector, PositiveNumber, UnitCartesianVector}
import domain.utils.InvalidArgumentError

sealed abstract case class Camera private[model] (
  location: CartesianVector,
  up: UnitCartesianVector,
  right: UnitCartesianVector,
  distanceFromScreen: PositiveNumber
) {
  def viewDirection: UnitCartesianVector = (up cross right) toUnitVector
}

object Camera {

  private val Epsilon = math.pow(10, -12)

  def apply(
    location: CartesianVector,
    up: CartesianVector,
    right: CartesianVector,
    distanceFromScreen: PositiveNumber
  ): Either[InvalidArgumentError, Camera] = {
    val upUnit = up.toUnitVector
    val rightUnit = right.toUnitVector
    if (math.abs((upUnit dot rightUnit) - 0) < Epsilon) {
      Right(new Camera(location, up.toUnitVector, right.toUnitVector, distanceFromScreen) {})
    } else {
      Left(InvalidArgumentError(s"up=$up and right=$right must be perpendicular"))
    }
  }
}
