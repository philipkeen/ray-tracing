package domain.model

import domain.maths.{CartesianVector, UnitCartesianVector}

final case class Hit(
  distanceFromRayOrigin: Double,
  intersectionPoint: CartesianVector,
  surfaceNormal: UnitCartesianVector,
//  reflectionDirection: UnitCartesianVector,
  reflectionRay: Option[ReflectedRay],
  refractionRay: Option[RefractedRay]
//  refractionDirection: Option[UnitCartesianVector]
)

final case class Reflection(
  ray: CartesianRay
)
