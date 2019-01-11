package domain.model

import domain.maths.CartesianVector.Origin
import domain.maths.PositiveInteger.One
import domain.maths.{CartesianVector, Proportion, Radians}
import org.scalatest.FunSuite
import test.VectorTest

class EllipseTest extends FunSuite with VectorTest {

  test("Ellipse.surfaceNormal should reflect orientation") {
    val toZ = Ellipse(Origin, One, One, Radians.Zero, Radians.Zero, Proportion.Zero, Colour.Black)
    val toX = Ellipse(Origin, One, One, Radians.Zero, Radians.QuarterTurn, Proportion.Zero, Colour.Black)
    val toY = Ellipse(Origin, One, One, Radians.QuarterTurn, Radians.QuarterTurn, Proportion.Zero, Colour.Black)
    val toNegativeX = Ellipse(Origin, One, One, Radians.HalfTurn, Radians.QuarterTurn, Proportion.Zero, Colour.Black)
    val toNegativeY = Ellipse(Origin, One, One, Radians.ThreeQuarterTurn, Radians.QuarterTurn, Proportion.Zero, Colour.Black)

    assert(negligibleDifference(toZ.surfaceNormal, CartesianVector.toUnitVector(0, 0, 1)))
    assert(negligibleDifference(toX.surfaceNormal, CartesianVector.toUnitVector(1, 0, 0)))
    assert(negligibleDifference(toY.surfaceNormal, CartesianVector.toUnitVector(0, 1, 0)))
    assert(negligibleDifference(toNegativeX.surfaceNormal, CartesianVector.toUnitVector(-1, 0, 0)))
    assert(negligibleDifference(toNegativeY.surfaceNormal, CartesianVector.toUnitVector(0, -1, 0)))
  }
}
