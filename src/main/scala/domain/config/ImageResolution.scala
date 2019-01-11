package domain.config

import domain.maths.PositiveInteger

final case class ImageResolution(
  width: PositiveInteger,
  height: PositiveInteger
)
