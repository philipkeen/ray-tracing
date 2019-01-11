package domain.model

import domain.maths.{CartesianVector, UnitCartesianVector}

final case class Contact(
  distanceFromRayOrigin: Double,
  intersectionPoint: CartesianVector,
  surfaceNormal: UnitCartesianVector,
  reflectionMaybe: Option[ReflectedRay],
  refractedRay: Option[RefractedRay]
)
