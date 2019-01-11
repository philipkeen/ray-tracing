package domain.maths

import org.scalatest.FunSuite

import scala.math._
import scala.util.Random

class CartesianVectorTest extends FunSuite {

  test("CartesianVector.dot") {
    val testVector = CartesianVector(1, 1, 1)
    0 to 50 foreach { _ =>
      val (x, y, z) = (Random.nextDouble(), Random.nextDouble(), Random.nextDouble())

      assert((testVector dot CartesianVector(x, y, z)) == (x + y + z))
    }
  }

  test("CartesianVector.cross") {
    val testX = CartesianVector(1, 0, 0)
    val testY = CartesianVector(0, 1, 0)
    val testZ = CartesianVector(0, 0, 1)
    0 to 50 foreach { _ =>
      val theta = Random.nextDouble() * 2 * Pi
      val r = Random.nextDouble()

      assert((testX cross (CartesianVector(0, sin(theta), cos(theta)) * r)) == (CartesianVector(0, -1 * cos(theta), sin(theta)) * r))
      assert((testY cross (CartesianVector(sin(theta), 0, cos(theta)) * r)) == (CartesianVector(cos(theta), 0, -1 * sin(theta)) * r))
      assert((testZ cross (CartesianVector(sin(theta), cos(theta), 0) * r)) == (CartesianVector(-1 * cos(theta), sin(theta), 0) * r))
    }
  }
}
