package domain

import domain.maths.{CartesianVector, Proportion}
import domain.model._
import domain.maths.PositiveInteger.One
import org.scalatest.FunSuite

class PhysicsAlgTest extends FunSuite {

  private val Epsilon = math.pow(10, -12)

  private val SqrtTwo = (One + One) ^ 0.5
  private val Two = One + One

  test("PhysicsAlg.contact returns ray's first hit with sphere") {
    val sphere =
      Sphere(
        centre = CartesianVector(4, 1, 4),
        radius = SqrtTwo,
        reflectivity = Proportion.Zero,
        refractiveIndex = RefractiveIndex.ofVacuum,
        transparency = Proportion.One,
        colour = Colour.Black
      )
    val fromOrigin = Ray(CartesianVector(-1, 1, -1), CartesianVector.toUnitVector(1, 0, 1))

    val actual = PhysicsAlg.contact(fromOrigin, sphere)

    assert(actual.nonEmpty)
    val hit = actual.get
    assert(negligibleDifference(hit.intersectionPoint, CartesianVector(3, 1, 3)))
    assert(negligibleDifference(hit.surfaceNormal, CartesianVector.toUnitVector(-1, 0, -1)))
    assert(hit.reflectionRay.isDefined)
    assert(negligibleDifference(hit.reflectionRay.get.ray.origin, CartesianVector(3, 1, 3)))
    assert(negligibleDifference(hit.reflectionRay.get.ray.direction, CartesianVector.toUnitVector(-1, 0, -1)))
    assert(math.abs(hit.distanceFromRayOrigin - math.sqrt(16 + 16)) < Epsilon)
  }

  test("PhysicsAlg.contact recognises miss") {
    val sphere =
      Sphere(
        centre = CartesianVector(2, 1, 4),
        radius = SqrtTwo,
        reflectivity = Proportion.Zero,
        refractiveIndex = RefractiveIndex.ofVacuum,
        transparency = Proportion.One,
        colour = Colour.Black
      )
    val fromOrigin = Ray(CartesianVector(1, 1, 0), CartesianVector.toUnitVector(-1, 0, 1))

    val actual = PhysicsAlg.contact(fromOrigin, sphere)

    assert(actual.isEmpty)
  }

  test("PhysicsAlg.contact correctly calculates reflection off sphere") {
    val sphere =
      Sphere(
        centre = CartesianVector(2, 1, 4),
        radius = SqrtTwo,
        reflectivity = Proportion.Zero,
        refractiveIndex = RefractiveIndex.ofVacuum,
        transparency = Proportion.One,
        colour = Colour.Black
      )
    val fromOrigin = Ray(CartesianVector(1, 1, 0), CartesianVector.toUnitVector(0, 0, 1))

    val actual = PhysicsAlg.contact(fromOrigin, sphere)

    assert(actual.nonEmpty)
    val hit = actual.get
    assert(negligibleDifference(hit.intersectionPoint, CartesianVector(1, 1, 3)))
    assert(negligibleDifference(hit.surfaceNormal, CartesianVector.toUnitVector(-1, 0, -1)))
    assert(hit.reflectionRay.isDefined)
    assert(negligibleDifference(hit.reflectionRay.get.ray.origin, CartesianVector(1, 1, 3)))
    assert(negligibleDifference(hit.reflectionRay.get.ray.direction, CartesianVector.toUnitVector(-1, 0, 0)))
    assert(math.abs(hit.distanceFromRayOrigin - 3) < Epsilon)
  }

  private def negligibleDifference(vector1: CartesianVector, vector2: CartesianVector): Boolean =
    (vector1 distanceTo vector2) < Epsilon
}
