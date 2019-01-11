package domain.model

import domain.maths.CartesianVector.Origin
import domain.maths.PositiveInteger.One
import domain.maths.{CartesianVector, Proportion, Radians}
import org.scalatest.FunSuite
import test.VectorTest

class EllipseTest extends FunSuite with VectorTest {

  test("Ellipse.surfaceNormal should reflect orientation") {
    val toZ = rotatedEllipse(Radians.Zero, Radians.Zero)
    val toX = rotatedEllipse(Radians.Zero, Radians.QuarterTurn)
    val toY = rotatedEllipse(Radians.QuarterTurn, Radians.QuarterTurn)
    val toNegativeX = rotatedEllipse(Radians.HalfTurn, Radians.QuarterTurn)
    val toNegativeY = rotatedEllipse(Radians.ThreeQuarterTurn, Radians.QuarterTurn)

    assert(negligibleDifference(toZ.surfaceNormal, CartesianVector.toUnitVector(0, 0, 1)))
    assert(negligibleDifference(toX.surfaceNormal, CartesianVector.toUnitVector(1, 0, 0)))
    assert(negligibleDifference(toY.surfaceNormal, CartesianVector.toUnitVector(0, 1, 0)))
    assert(negligibleDifference(toNegativeX.surfaceNormal, CartesianVector.toUnitVector(-1, 0, 0)))
    assert(negligibleDifference(toNegativeY.surfaceNormal, CartesianVector.toUnitVector(0, -1, 0)))
  }

  private def rotatedEllipse(horizontalTurn: Radians, verticalTilt: Radians): Ellipse =
    Ellipse(
      centre = Origin,
      axis0Length = One,
      axis1Length = One,
      horizontalTurn = horizontalTurn,
      verticalTilt = verticalTilt,
      reflectivity = Proportion.Zero,
      transparency = Proportion.Zero,
      colour = Colour.Black
    )
}
