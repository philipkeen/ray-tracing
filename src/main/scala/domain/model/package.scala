package domain

import domain.maths.{CartesianVector, SphericalCoordinateVector, UnitCartesianVector, UnitSphericalCoordinateVector}

package object model {

  type CartesianRay = Ray[CartesianVector, UnitCartesianVector]

  type SphericalRay = Ray[SphericalCoordinateVector, UnitSphericalCoordinateVector]
}
