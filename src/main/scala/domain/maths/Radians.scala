package domain.maths

import scala.math._

sealed abstract case class Radians private[maths] (value: Double) { self =>

  def +(that: Radians): Radians = Radians(self.value + that.value)

  def -(that: Radians): Radians = Radians(self.value - that.value)

  def *(d: Double): Radians = Radians(self.value * d)

  def /(d: Double): Radians = Radians(self.value / d)
}

object Radians {
  def apply(value: Double): Radians = {
    if (value >= 0) {
      new Radians(value % (2 * Pi)) {}
    } else {
      new Radians(2 * Pi + (value % (2 * Pi))) {}
    }
  }

  val Zero: Radians = apply(0)

  val HalfTurn: Radians = apply(Pi)

  val QuarterTurn: Radians = apply(Pi / 2)

  val ThreeQuarterTurn: Radians = apply(3 * Pi / 2)

  val EighthOfATurn: Radians = apply(Pi / 4)
}
