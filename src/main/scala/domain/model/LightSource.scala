package domain.model

import domain.maths.{CartesianVector, NonNegativeNumber, Proportion}

final case class LightSource(
  location: CartesianVector,
  radius: NonNegativeNumber,
  ambience: Proportion
)
