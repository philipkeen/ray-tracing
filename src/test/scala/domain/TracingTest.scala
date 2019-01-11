package domain

import domain.config.RenderSettings
import domain.maths._
import domain.model._
import org.scalatest.FunSuite
import test.TestRandom

class TracingTest extends FunSuite {

  val testObject = Tracing(new TestRandom)

  test("Tracing.castRay includes reflections"){
    val renderSettings =
      RenderSettings(
        maxReflections = PositiveInteger.One,
        maxShadowRaysPerPoint = PositiveInteger.One,
        defaultColour = Colour.White
      ).right.get

    val redMirror: Set[Shape] =
      Set(
        Triangle(
          vertex0 = CartesianVector(-2, 8, 2),
          vertex1 = CartesianVector(2, 4, 2),
          vertex2 = CartesianVector(2, 4, -2),
          reflectivity = Proportion(0.8).right.get,
          transparency = Proportion.Zero,
          colour = Colour(125, 0, 0).right.get
        ).right.get,
        Triangle(
          vertex0 = CartesianVector(-2, 8, 2),
          vertex1 = CartesianVector(-2, 8, -2),
          vertex2 = CartesianVector(2, 4, -2),
          reflectivity = Proportion(0.8).right.get,
          transparency = Proportion.Zero,
          colour = Colour(125, 0, 0).right.get
        ).right.get
      )

    val greenWall: Set[Shape] =
      Set(
        Triangle(
          vertex0 = CartesianVector(-4, 8, 2),
          vertex1 = CartesianVector(-4, 4, 2),
          vertex2 = CartesianVector(-8, 4, -2),
          reflectivity = Proportion.Zero,
          transparency = Proportion.Zero,
          colour = Colour(0, 255, 0).right.get
        ).right.get,
        Triangle(
          vertex0 = CartesianVector(-4, 8, 2),
          vertex1 = CartesianVector(-4, 8, -2),
          vertex2 = CartesianVector(-8, 4, -2),
          reflectivity = Proportion.Zero,
          transparency = Proportion.Zero,
          colour = Colour(0, 255, 0).right.get
        ).right.get
      )

    val actual =
      testObject.castRay(
        ray = Ray(CartesianVector.Origin, CartesianVector.toUnitVector(0, 1, 0)),
        lightSource = LightSource(CartesianVector(-6, 0, 0), NonNegativeNumber.Zero, Proportion.Zero),
        objects = redMirror ++ greenWall,
        renderSettings = renderSettings
      )

    assert(actual == Colour(24, 144, 0).right.get)
  }

  test("Tracing.castRay includes refractions") {
    val renderSettings =
      RenderSettings(
        maxReflections = PositiveInteger.One,
        maxShadowRaysPerPoint = PositiveInteger.One,
        defaultColour = Colour.White
      ).right.get

    val sphere: Set[Shape] =
      Set(
        Sphere(
          centre = CartesianVector(0, 10, -2),
          radius = PositiveNumber(5).right.get,
          refractiveIndex = RefractiveIndex(1.3).right.get,
          reflectivity = Proportion(0.2).right.get,
          transparency = Proportion(0.5).right.get,
          colour = Colour.White
        )
      )

    val actual =
      testObject.castRay(
        ray = Ray(CartesianVector.Origin, CartesianVector.toUnitVector(0, 1, 0)),
        lightSource = LightSource(CartesianVector(-6, 0, 0), NonNegativeNumber.Zero, Proportion.Zero),
        objects = sphere,
        renderSettings = renderSettings
      )
    assert(actual == Colour(203, 203, 203).right.get)
  }

}
