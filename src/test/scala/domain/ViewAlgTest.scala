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

//    Map(
//      Pixel(0, 0) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.6, 1, 0.8).toUnitVector),
//      Pixel(0, 1) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.6, 1, 0.4).toUnitVector),
//      Pixel(0, 2) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.6, 1, 0).toUnitVector),
//      Pixel(0, 3) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.6, 1, -0.4).toUnitVector),
//      Pixel(0, 4) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.6, 1, -0.8).toUnitVector),
//      Pixel(1, 0) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.2, 1, 0.8).toUnitVector),
//      Pixel(1, 1) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.2, 1, 0.4).toUnitVector),
//      Pixel(1, 2) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.2, 1, 0).toUnitVector),
//      Pixel(1, 3) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.2, 1, -0.4).toUnitVector),
//      Pixel(1, 4) -> Ray(CartesianVector(0, -5, 0), CartesianVector(-0.2, 1, -0.8).toUnitVector),
//      Pixel(2, 0) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.2, 1, 0.8).toUnitVector),
//      Pixel(2, 1) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.2, 1, 0.4).toUnitVector),
//      Pixel(2, 2) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.2, 1, 0).toUnitVector),
//      Pixel(2, 3) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.2, 1, -0.4).toUnitVector),
//      Pixel(2, 4) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.2, 1, -0.8).toUnitVector),
//      Pixel(3, 0) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.6, 1, 0.8).toUnitVector),
//      Pixel(3, 1) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.6, 1, 0.4).toUnitVector),
//      Pixel(3, 2) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.6, 1, 0).toUnitVector),
//      Pixel(3, 3) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.6, 1, -0.4).toUnitVector),
//      Pixel(3, 4) -> Ray(CartesianVector(0, -5, 0), CartesianVector(0.6, 1, -0.8).toUnitVector)
//    ).foreach { case (pixel, expectedRay) =>
//      assert(ViewAlg.getCameraRayThroughPixel(pixel, imageSettings) == expectedRay)
//    }
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
