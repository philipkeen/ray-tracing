package domain.model

import domain.maths.{PositiveInteger, PositiveNumber}

final case class Screen(
  screenWidthResolution: PositiveInteger,
  screenHeightResolution: PositiveInteger,
  pixelLength: PositiveNumber
)
