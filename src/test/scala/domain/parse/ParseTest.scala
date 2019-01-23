package domain.parse

import domain.maths.{CartesianVector, PositiveNumber, Proportion}
import domain.model.{Colour, Sphere, Triangle}
import org.scalatest.FunSuite

class ParseTest extends FunSuite {

  test("parse.parseFileContents should parse a scene") {
    val fileContents = List(
      "lightSource",
      "  location: (0, -200, 0)",
      "  radius: 10.25",
      "  ambience: 0.2",
      "renderSettings",
      "  maxReflections: 2",
      "  maxShadowRaysPerPoint: 2",
      "  antiAliasing: true",
      "  defaultColour: ffffff",
      "triangle",
      "  vertex0: (1, 1, 1)",
      "  vertex1: (1, 2, 3)",
      "  vertex2: (3, 2, 1)",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  colour: ffffff",
      "sphere",
      "  centre: (10, 10, 10)",
      "  radius: 30.5",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  refractiveIndex: 2.5",
      "  colour: 014e5a",
      "cone",
      "  apex: (10, 10, 10)",
      "  endRadius: 30.5",
      "  height: 8",
      "  type: open",
      "  horizontalTurn: 0",
      "  verticalTilt: 1.570796326794897",
      "  reflectivity: 0.8",
      "  transparency: 0.2",
      "  refractiveIndex: 2.5",
      "  colour: 014e5a",
      "cylinder",
      "  centre: (10, 10, 10)",
      "  radius: 25",
      "  length: 8",
      "  type: open",
      "  horizontalTurn: 0",
      "  verticalTilt: 1.570796326794897",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  refractiveIndex: 2.5",
      "  colour: 014e5a"
    )

    val actual = parseFileContents(fileContents)

    assert(actual.isRight)
    assert(actual.right.get.lightSource.location == CartesianVector(0, -200, 0))
    assert(actual.right.get.lightSource.radius.value == PositiveNumber(10.25).right.get.value)
  }

  test("parse.parsefileContents should ignore content-less lines") {
    val fileContents = List(
      "lightSource",
      "  location: (0, -200, 0)",
      "  radius: 10.25",
      "  ambience: 0.2",
      "  ",
      "renderSettings",
      "  maxReflections: 2",
      "  maxShadowRaysPerPoint: 2",
      "  antiAliasing: true",
      "  defaultColour: ffffff",
      "triangle",
      "  ",
      "  vertex0: (1, 1, 1)",
      "  vertex1: (1, 2, 3)",
      "  vertex2: (3, 2, 1)",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  colour: ffffff",
      "sphere",
      "  centre: (10, 10, 10)",
      "  radius: 30.5",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  refractiveIndex: 2.5",
      "  colour: 014e5a"
    )

    assert(parseFileContents(fileContents).isRight)
  }

  test("parse.parsefileContents should report bad vector") {
    val fileContents = List(
      "lightSource",
      "  location: (0, -200, 0)",
      "  radius: 10.25",
      "  ambience: 0.2",
      "  ",
      "renderSettings",
      "  maxReflections: 2",
      "  maxShadowRaysPerPoint: 2",
      "  antiAliasing: true",
      "  defaultColour: ffffff",
      "triangle",
      "  ",
      "  vertex0: (1, 1, 1)",
      "  vertex1: (1, 2, 3)",
      "  vertex2: (3, 2)",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  colour: ffffff",
      "sphere",
      "  centre: (10, 10, 10)",
      "  radius: 30.5",
      "  reflectivity: 0.8",
      "  transparency: 0",
      "  refractiveIndex: 2.5",
      "  colour: 014e5a"
    )

    val actual = parseFileContents(fileContents)

    assert(actual.isLeft)
    assert(actual.left.get.message == "Error in line 15: Unable to interpret (3, 2) as a vector with three coordinates")
  }
}
