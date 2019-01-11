package domain.model

import domain.maths.{CartesianVector, UnitCartesianVector}

final case class Hit(
  distanceFromRayOrigin: Double,
  intersectionPoint: CartesianVector,
  surfaceNormal: UnitCartesianVector,
  reflectionRay: Option[ReflectedRay],
  refractionRay: Option[RefractedRay]
)

final case class Reflection(
  ray: CartesianRay
)
