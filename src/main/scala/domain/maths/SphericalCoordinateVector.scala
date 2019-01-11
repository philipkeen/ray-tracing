package domain.maths

import domain.maths.NonNegativeNumber.absoluteValue

import scala.math._

sealed abstract class SphericalCoordinateVector extends Vector3D { self =>

  def radius: NonNegativeNumber
  def horizontalAngle: Radians
  def verticalAngle: Radians

  def toCartesianVector: CartesianVector =
    CartesianVector(
      x = radius.value * cosine(horizontalAngle) * sine(verticalAngle),
      y = radius.value * sine(horizontalAngle) * sine(verticalAngle),
      z = radius.value * cosine(verticalAngle)
    )

  def toUnitVector: UnitSphericalCoordinateVector =
    new UnitSphericalCoordinateVector(horizontalAngle, verticalAngle) {}
}

sealed abstract case class ArbitrarySphericalCoordinateVector(
  override val radius: NonNegativeNumber,
  override val horizontalAngle: Radians,
  override val verticalAngle: Radians
) extends SphericalCoordinateVector

sealed abstract case class UnitSphericalCoordinateVector(
  override val horizontalAngle: Radians,
  override val verticalAngle: Radians
) extends SphericalCoordinateVector with UnitVector {
  override val radius: NonNegativeNumber = PositiveInteger.One

  override def toCartesianVector: UnitCartesianVector =
    CartesianVector.toUnitVector(
      x = cosine(horizontalAngle) * sine(verticalAngle),
      y = sine(horizontalAngle) * sine(verticalAngle),
      z = cosine(verticalAngle)
    )
}

object SphericalCoordinateVector {

  def apply(radius: Double, horizontalAngle: Radians, verticalAngle: Radians): SphericalCoordinateVector = {
    import Radians._
    (horizontalAngle, verticalAngle, radius) match {
      case (h, v, r) if r < 0 && v.value > Pi => new ArbitrarySphericalCoordinateVector(absoluteValue(r), h, Zero - v) {}
      case (h, v, r) if r < 0 => new ArbitrarySphericalCoordinateVector(absoluteValue(r), h + HalfTurn, v) {}
      case (h, v, r) if v.value > Pi => new ArbitrarySphericalCoordinateVector(absoluteValue(r), h + HalfTurn, Zero - v) {}
      case (h, v, r) => new ArbitrarySphericalCoordinateVector(absoluteValue(r), h, v) {}
    }
  }

  def toUnitVector(horizontalAngle: Radians, verticalAngle: Radians): UnitSphericalCoordinateVector = {
    import Radians._
    (horizontalAngle, verticalAngle) match {
      case (h, v) if v.value > Pi => new UnitSphericalCoordinateVector(h + HalfTurn, Zero - v) {}
      case (h, v) => new UnitSphericalCoordinateVector(h, v) {}
    }
  }
}
