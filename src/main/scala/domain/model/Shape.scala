package domain.model

import domain.maths._
import domain.utils.InvalidArgumentError

sealed trait Shape {
  def reflectivity: Proportion
  def transparency: Proportion
  def colour: Colour
}

sealed trait PlanarShape extends Shape {
  def surfaceNormal: UnitCartesianVector
}

sealed abstract case class Triangle private[model] (
  vertex0: CartesianVector,
  vertex1: CartesianVector,
  vertex2: CartesianVector,
  override val reflectivity: Proportion,
  override val transparency: Proportion,
  override val colour: Colour
) extends PlanarShape {

  override val surfaceNormal: UnitCartesianVector =
    ((vertex1 - vertex0) cross (vertex2 - vertex0)).toUnitVector
}

object Triangle {
  def apply(
    vertex0: CartesianVector,
    vertex1: CartesianVector,
    vertex2: CartesianVector,
    reflectivity: Proportion,
    transparency: Proportion,
    colour: Colour
  ): Either[InvalidArgumentError, Triangle] =
    if (vertex0 != vertex1 && vertex0 != vertex2 && vertex1 != vertex2) {
      Right(
        new Triangle(
          vertex0 = vertex0,
          vertex1 = vertex1,
          vertex2 = vertex2,
          reflectivity = reflectivity,
          transparency = transparency,
          colour = colour
        ) {}
      )
    } else Left(InvalidArgumentError(s"Error creating triangle on $vertex0, $vertex1, $vertex2 - vertices must be distinct"))
}

//final case class Disc(
//  centre: CartesianVector,
//  radius: PositiveNumber,
//  horizontalTurn: Radians,
//  verticalTilt: Radians,
//  override val reflectivity: Proportion,
//  override val colour: Colour
//) extends PlanarShape {
//
//  override val surfaceNormal: UnitCartesianVector =
//    SphericalCoordinateVector.toUnitVector(horizontalTurn, verticalTilt).toCartesianVector.toUnitVector
//}

final case class Ellipse(
  centre: CartesianVector,
  axis0Length: PositiveNumber,
  axis1Length: PositiveNumber,
  horizontalTurn: Radians,
  verticalTilt: Radians,
  override val reflectivity: Proportion,
  override val transparency: Proportion,
  override val colour: Colour
) extends PlanarShape {

  override val surfaceNormal: UnitCartesianVector =
    SphericalCoordinateVector.toUnitVector(horizontalTurn, verticalTilt).toCartesianVector
}

object Disc {
  def apply(
    centre: CartesianVector,
    radius: PositiveNumber,
    horizontalTurn: Radians,
    verticalTilt: Radians,
    reflectivity: Proportion,
    transparency: Proportion,
    colour: Colour
  ): Ellipse =
    Ellipse(
      centre = centre,
      axis0Length = radius,
      axis1Length = radius,
      horizontalTurn = horizontalTurn,
      verticalTilt = verticalTilt,
      reflectivity = reflectivity,
      transparency = transparency,
      colour = colour
    )
}

sealed trait SolidShape extends Shape {
  def refractiveIndex: RefractiveIndex
}

final case class Ellipsoid(
  centre: CartesianVector,
  axis0Length: PositiveNumber,
  axis1Length: PositiveNumber,
  axis2Length: PositiveNumber,
  horizontalTurn: Radians,
  verticalTilt: Radians,
  override val reflectivity: Proportion,
  override val refractiveIndex: RefractiveIndex,
  override val transparency: Proportion,
  override val colour: Colour
) extends SolidShape

sealed trait CylinderType

object CylinderType {

  case object Open extends CylinderType
  case object HalfOpen extends CylinderType
  case object Closed extends CylinderType

  private val stringMappings = Map(
    "open" -> Open,
    "half-open" -> HalfOpen,
    "closed" -> Closed
  )

  val recognisedInputs: Set[String] = stringMappings.keySet

  def fromString(string: String): Option[CylinderType] = stringMappings.get(string)
}

final case class Cylinder(
  centre: CartesianVector,
  radius: PositiveNumber,
  length: PositiveNumber,
  cylinderType: CylinderType,
  horizontalTurn: Radians,
  verticalTilt: Radians,
  override val reflectivity: Proportion,
  override val refractiveIndex: RefractiveIndex,
  override val transparency: Proportion,
  override val colour: Colour
) extends SolidShape

sealed trait ConeType

object ConeType {

  case object Open extends ConeType
  case object Closed extends ConeType

  private val stringMappings = Map(
    "open" -> Open,
    "closed" -> Closed
  )

  val recognisedInputs: Set[String] = stringMappings.keySet

  def fromString(string: String): Option[ConeType] = stringMappings.get(string)
}

final case class Cone(
  apex: CartesianVector,
  endRadius: PositiveNumber,
  height: PositiveNumber,
  coneType: ConeType,
  horizontalTurn: Radians,
  verticalTilt: Radians,
  override val reflectivity: Proportion,
  override val refractiveIndex: RefractiveIndex,
  override val transparency: Proportion,
  override val colour: Colour
) extends SolidShape

object Sphere {
  def apply(
    centre: CartesianVector,
    radius: PositiveNumber,
    reflectivity: Proportion,
    refractiveIndex: RefractiveIndex,
    transparency: Proportion,
    colour: Colour
  ): Ellipsoid =
    Ellipsoid(
      centre = centre,
      axis0Length = radius,
      axis1Length = radius,
      axis2Length = radius,
      horizontalTurn = Radians.Zero,
      verticalTilt = Radians.Zero,
      reflectivity = reflectivity,
      refractiveIndex = refractiveIndex,
      transparency = transparency,
      colour = colour
    )
}

object Spheroid {
  def apply(
    centre: CartesianVector,
    axis0Length: PositiveNumber,
    axis1Length: PositiveNumber,
    horizontalTurn: Radians,
    verticalTilt: Radians,
    reflectivity: Proportion,
    refractiveIndex: RefractiveIndex,
    transparency: Proportion,
    colour: Colour
  ): Ellipsoid =
    Ellipsoid(
      centre = centre,
      axis0Length = axis0Length,
      axis1Length = axis1Length,
      axis2Length = axis1Length,
      horizontalTurn = horizontalTurn,
      verticalTilt = verticalTilt,
      reflectivity = reflectivity,
      refractiveIndex = refractiveIndex,
      transparency = transparency,
      colour = colour
    )

  def apply(
    centre: CartesianVector,
    axis0Length: PositiveNumber,
    axis1Length: PositiveNumber,
    reflectivity: Proportion,
    refractiveIndex: RefractiveIndex,
    transparency: Proportion,
    colour: Colour
  ): Ellipsoid =
    Ellipsoid(
      centre = centre,
      axis0Length = axis0Length,
      axis1Length = axis1Length,
      axis2Length = axis1Length,
      horizontalTurn = Radians.Zero,
      verticalTilt = Radians.Zero,
      reflectivity = reflectivity,
      refractiveIndex = refractiveIndex,
      transparency = transparency,
      colour = colour
    )
}

object Ellipsoid {
  def apply(
    centre: CartesianVector,
    axis0Length: PositiveNumber,
    axis1Length: PositiveNumber,
    axis2Length: PositiveNumber,
    reflectivity: Proportion,
    refractiveIndex: RefractiveIndex,
    transparency: Proportion,
    colour: Colour
  ): Ellipsoid =
    Ellipsoid(
      centre = centre,
      axis0Length = axis0Length,
      axis1Length = axis1Length,
      axis2Length = axis2Length,
      horizontalTurn = Radians.Zero,
      verticalTilt = Radians.Zero,
      reflectivity = reflectivity,
      refractiveIndex = refractiveIndex,
      transparency = transparency,
      colour = colour
    )
}
