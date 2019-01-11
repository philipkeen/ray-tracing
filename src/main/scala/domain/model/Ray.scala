package domain.model

import domain.maths.{UnitVector, Vector3D}

final case class Ray[V <: Vector3D, U <: V with UnitVector](origin: V, direction: U)
