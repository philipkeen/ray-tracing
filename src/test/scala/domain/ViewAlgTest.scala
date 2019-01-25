package domain

import domain.config.ImageResolution
import domain.maths.{CartesianVector, PositiveInteger, PositiveNumber}
import domain.model.{Camera, Pixel, Ray, Screen}
import org.scalatest.FunSuite

class ViewAlgTest extends FunSuite {

  test("ViewAlg.getCameraRayThroughPixel") {
    val resolution = ImageResolution(
      width = PositiveInteger(4).right.get,
      height = PositiveInteger(5).right.get
    )

    val expectedRay = Ray(CartesianVector.Origin, CartesianVector(-0.6, 1, 0.8).toUnitVector)

    assert(ViewAlg.getCameraRayThroughPixel(Pixel(0, 0), resolution) == expectedRay)
  }

  test("ViewAlg.getCameraRaysThroughPixel") {
    val resolution = ImageResolution(
      width = PositiveInteger(4).right.get,
      height = PositiveInteger(5).right.get
    )

    val expected =
      Vector(
        Ray(CartesianVector.Origin, CartesianVector.toUnitVector(-0.7, 1, 0.9)),
        Ray(CartesianVector.Origin, CartesianVector.toUnitVector(-0.5, 1, 0.9)),
        Ray(CartesianVector.Origin, CartesianVector.toUnitVector(-0.6, 1, 0.8)),
        Ray(CartesianVector.Origin, CartesianVector.toUnitVector(-0.7, 1, 0.7)),
        Ray(CartesianVector.Origin, CartesianVector.toUnitVector(-0.5, 1, 0.7))
      )

    assert(ViewAlg.getCameraRaysThroughPixel(Pixel(0, 0), resolution) == expected)
  }
}
