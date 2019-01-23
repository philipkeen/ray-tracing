package domain.config

import domain.error.InvalidArgumentError
import domain.maths.PositiveInteger
import domain.model.Colour

sealed abstract case class RenderSettings private(
  maxReflections: PositiveInteger,
  maxShadowRaysPerPoint: PositiveInteger,
  antiAliasing: Boolean,
  defaultColour: Colour
)

object RenderSettings {
  private val reflectionLimit = 15

  def apply(
    maxReflections: PositiveInteger,
    maxShadowRaysPerPoint: PositiveInteger,
    antiAliasing: Boolean = false,
    defaultColour: Colour = Colour.White
  ): Either[InvalidArgumentError, RenderSettings] =
    if (maxReflections <= reflectionLimit) {
      Right(
        new RenderSettings(maxReflections, maxShadowRaysPerPoint, antiAliasing, defaultColour) {}
      )
    } else {
      Left(
        InvalidArgumentError(
          s"Unable to create RenderSettings: $maxReflections is above the permitted limit of $reflectionLimit for maxReflections"
        )
      )
    }
}
