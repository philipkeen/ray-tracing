package domain.instances

import domain.maths._
import domain.model._

trait DoubleInstances {
  implicit class DoubleInstance(double: Double) {
    def -(proportion: Proportion): Double = double - proportion.value

    def *[R <: RealNumber](r: R): Double = double * r.value

    def *(p: Proportion): Double = p.value * double

    def *(vector: CartesianVector): CartesianVector = vector * double

    def *(radians: Radians): Radians = radians * double

    def /[P <: PositiveNumber](p: P): Double = double / p.value

    def <[R <: RealNumber](r: R): Boolean = double < r.value

    def <=[R <: RealNumber](r: R): Boolean = double <= r.value

    def >[R <: RealNumber](r: R): Boolean = double > r.value

    def >=[R <: RealNumber](r: R): Boolean = double >= r.value
  }
}
