package domain.model

import java.awt.Color

import domain.error.InvalidArgumentError
import domain.maths.Proportion

sealed abstract case class PrimaryColour private[model](value: Int) { self =>
  import PrimaryColour._

  private def newIntensity(changeBy: Double): Int =
    math.max(minValue, math.min(maxValue, (value * changeBy).toInt))

  def *(by: Double): PrimaryColour =
    new PrimaryColour(newIntensity(by)) {}

  def +(that: PrimaryColour): PrimaryColour =
    if (self.value + that.value > maxValue) {
      new PrimaryColour(maxValue) {}
    } else {
      new PrimaryColour(self.value + that.value) {}
    }
}

object PrimaryColour {
  val maxValue = 255
  val minValue = 0

  val Zero: PrimaryColour = new PrimaryColour(minValue) {}
  def Max: PrimaryColour = new PrimaryColour(maxValue) {}

  def apply(value: Int, name: String): Either[InvalidArgumentError, PrimaryColour] =
    if (value > maxValue || value < minValue) {
      Left(InvalidArgumentError(s"$value for $name lies outside [0, 255]"))
    } else {
      Right(new PrimaryColour(value) {})
    }

  def min(primary1: PrimaryColour, primary2: PrimaryColour): PrimaryColour =
    new PrimaryColour(math.min(primary1.value, primary1.value)) {}

  def averagePrimary(colours: List[PrimaryColour]): PrimaryColour = {
    if (colours.isEmpty) {
      Zero
    } else {
      val meanValue = colours.map(_.value).sum.toDouble / colours.size
      new PrimaryColour(meanValue.toInt) {}
    }
  }
}

sealed abstract case class Colour private (red: PrimaryColour, green: PrimaryColour, blue: PrimaryColour) { self =>
  import PrimaryColour._

  def toJavaColor: Color = new Color(red.value, green.value, blue.value)

  def *(increaseBy: Double): Colour = new Colour(red * increaseBy, green * increaseBy, blue * increaseBy) {}

  def *(increaseBy: Proportion): Colour = new Colour(red * increaseBy.value, green * increaseBy.value, blue * increaseBy.value) {}

  def +(that: Colour): Colour = new Colour(red + that.red, green + that.green, blue + that.blue) {}

  def filter(that: Colour): Colour =
    new Colour(
      red * (that.red.value / maxValue),
      green * (that.green.value / maxValue),
      blue * (that.blue.value / maxValue)
    ) {}
}

object Colour {
  import PrimaryColour._

  def apply(redValue: Int, greenValue: Int, blueValue: Int): Either[InvalidArgumentError, Colour] =
    for {
      red <- PrimaryColour(redValue, "red")
      green <- PrimaryColour(greenValue, "green")
      blue <- PrimaryColour(blueValue, "blue")
    } yield new Colour(red, green, blue) {}

  val Black: Colour = new Colour(Zero, Zero, Zero) {}
  val White: Colour = new Colour(Max, Max, Max) {}

  def min(colour1: Colour, colour2: Colour): Colour =
    new Colour(
      red = PrimaryColour.min(colour1.red, colour2.red),
      green = PrimaryColour.min(colour1.green, colour2.green),
      blue = PrimaryColour.min(colour1.blue, colour2.blue),
    ) {}

  def average(colours: List[Colour]): Colour =
    new Colour(
      red = averagePrimary(colours.map(_.red)),
      green = averagePrimary(colours.map(_.green)),
      blue = averagePrimary(colours.map(_.blue))
    ) {}
}
