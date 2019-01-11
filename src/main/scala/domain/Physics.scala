package domain

import domain.instances._
import domain.maths._
import domain.maths.NonNegativeNumber.square
import domain.maths.PositiveInteger.One
import domain.model._

object Physics {

  private implicit val Epsilon: Double = math.pow(10, -12)

  private val Y = CartesianVector.toUnitVector(0, 1, 0)
  private val Z = CartesianVector.toUnitVector(0, 0, 1)

  def contact(ray: CartesianRay, shape: Shape): Option[Hit] = {
    shape match {
      case planarShape: PlanarShape => contactWith2DShape(ray, planarShape)
      case solidShape: SolidShape => contactWith3DShapeSurface(ray, solidShape)
    }
  }

  private def contactWith2DShape(ray: CartesianRay, shape: PlanarShape): Option[Hit] = {
    val surfacePointMaybe = shape match {
      case triangle: Triangle => intersectionWithTriangle(ray, triangle)
      case ellipse: Ellipse => intersectionWithEllipse(ray, ellipse)
    }
    surfacePointMaybe.map { surfacePoint =>
      Hit(
        distanceFromRayOrigin = ray.origin distanceTo surfacePoint.point,
        intersectionPoint = surfacePoint.point,
        surfaceNormal = surfacePoint.normal,
        reflectionRay = Some(reflectionAtSurface(surfacePoint, ray.direction)),
        refractionRay = None
      )
    }
  }

  private def contactWith3DShapeSurface(ray: CartesianRay, shape: SolidShape): Option[Hit] =
    (shape match {
      case ellipsoid: Ellipsoid => intersectionWithEllipsoid(ray, ellipsoid)
      case cylinder: Cylinder => intersectionWithCylinder(ray, cylinder)
      case cone: Cone => intersectionWithCone(ray, cone)
    })
      .map { surfacePoint =>
        val (refractionMaybe, reflectionMaybe) =
          refractionAndReflectionAtSurface(surfacePoint, ray.direction, shape.refractiveIndex)
        (refractionMaybe, reflectionMaybe, surfacePoint)
      }
      .map { case (refractionMaybe, reflectionMaybe, surfacePoint) =>
        Hit(
          distanceFromRayOrigin = ray.origin distanceTo surfacePoint.point,
          intersectionPoint = surfacePoint.point,
          surfaceNormal = surfacePoint.normal,
          reflectionRay = reflectionMaybe,
          refractionRay = refractionMaybe
        )
      }

  private def intersectionWithTriangle(ray: CartesianRay, triangle: Triangle): Option[SurfacePoint] = {
    import triangle._

    checkRayNotParallelWithShape(ray, triangle) {
      checkRayHitsShape(ray, triangle) { intersectionWithPlane =>
        // start at vertex 0 and move clockwise around the edges of the triangle - if the contact with the plane is
        // always on your right then it is within the triangle
        val sideCheck1 = ((vertex1 - vertex0) cross (intersectionWithPlane - vertex0)) dot surfaceNormal
        val sideCheck2 = ((vertex2 - vertex1) cross (intersectionWithPlane - vertex1)) dot surfaceNormal
        val sideCheck3 = ((vertex0 - vertex2) cross (intersectionWithPlane - vertex2)) dot surfaceNormal
        sideCheck1 >= 0 && sideCheck2 >= 0 && sideCheck3 >= 0
      }
    } map { SurfacePoint(_, surfaceNormal) }
  }

  private def intersectionWithEllipse(ray: CartesianRay, ellipse: Ellipse): Option[SurfacePoint] = {
    import ellipse._

    convertFromSolutionUsingEllipseAlignedToXYPlane(ray, ellipse) { case (rotatedRay, alignedEllipse) =>
      checkRayNotParallelWithShape(rotatedRay, alignedEllipse) {
        checkRayHitsShape(rotatedRay, alignedEllipse) { intersectionWithPlane =>
          val x = intersectionWithPlane.x - alignedEllipse.centre.x
          val y = intersectionWithPlane.y - alignedEllipse.centre.y
          square(x / axis0Length) + square(y / axis1Length) <= 1
        }
      }
    } map { SurfacePoint(_, surfaceNormal) }
  }

  private def intersectionWithEllipsoid(ray: CartesianRay, ellipsoid: Ellipsoid): Option[SurfacePoint] = {
    import ellipsoid._

    convertFromSolutionUsingShiftedRay(ray, -1 * centre) { shiftedRay =>
      convertFromSolutionUsingRayRotatedAroundOrigin(shiftedRay, -1 * horizontalTurn, -1 * verticalTilt) { rotatedRay =>
        convertFromSolutionUsingScaledRay(rotatedRay, One / axis2Length, One / axis1Length, One / axis0Length) { scaledRay =>
          findIntersectionsWithUnitSphere(scaledRay)
        }
      }
    }.toList
      .sortBy(surfacePoint => (ray.origin - surfacePoint.point).magnitude)
      .headOption
  }

  private def intersectionWithCylinder(ray: CartesianRay, cylinder: Cylinder): Option[SurfacePoint] = {
    import cylinder._

    convertFromSolutionUsingShiftedRay(ray, -1 * centre) { shiftedRay =>
      convertFromSolutionUsingRayRotatedAroundOrigin(shiftedRay, -1 * horizontalTurn, -1 * verticalTilt) { rotatedRay =>
        convertFromSolutionUsingScaledRay(rotatedRay, One / radius, One / radius, One) { scaledRay =>
          val endCapIntersections = cylinder.cylinderType match {
            case CylinderType.Open => Set.empty[SurfacePoint]
            case CylinderType.HalfOpen => findIntersectionWithUnitEndCap(scaledRay, -1 * length / 2).toSet
            case CylinderType.Closed => Set(-1 * length / 2, length / 2).flatMap(findIntersectionWithUnitEndCap(scaledRay, _))
          }
          findIntersectionsWithUnitCylinder(scaledRay, length) union endCapIntersections
        }
      }
    }.toList
      .sortBy(surfacePoint => (ray.origin - surfacePoint.point).magnitude)
      .headOption
  }

  private def intersectionWithCone(ray: CartesianRay, cone: Cone): Option[SurfacePoint] = {
    import cone._

    convertFromSolutionUsingShiftedRay(ray, -1 * apex) { shiftedRay =>
      convertFromSolutionUsingRayRotatedAroundOrigin(shiftedRay, -1 * horizontalTurn, -1 * verticalTilt) { rotatedRay =>
        convertFromSolutionUsingScaledRay(rotatedRay, One / endRadius, One / endRadius, One) { scaledRay =>
          val endCapIntersections = cone.coneType match {
            case ConeType.Open => Set.empty[SurfacePoint]
            case ConeType.Closed => Set(height.value).flatMap(findIntersectionWithUnitEndCap(scaledRay, _))
          }
          findIntersectionsWithUnitCone(scaledRay, height) union endCapIntersections
        }
      }
    }.toList
      .sortBy(surfacePoint => (ray.origin - surfacePoint.point).magnitude)
      .headOption
  }

  private def checkRayNotParallelWithShape(
    ray: CartesianRay,
    shape: PlanarShape
  )(
    f: => Option[CartesianVector]
  ): Option[CartesianVector] =
    if (math.abs(shape.surfaceNormal dot ray.direction) < Epsilon) {
      None
    } else {
      f
    }

  private def checkRayHitsShape(
    ray: CartesianRay,
    shape: PlanarShape
  )(
    isCoplanarPointInShape: CartesianVector => Boolean
  ): Option[CartesianVector] = {
    val pointOnShape =
      shape match {
        case t: Triangle => t.vertex0
        case e: Ellipse => e.centre
      }
    intersectionWithPlane(ray, shape.surfaceNormal, pointOnShape)
      .filter(isCoplanarPointInShape)
  }

  private def intersectionWithPlane(
    ray: CartesianRay,
    planeNormal: UnitCartesianVector,
    pointOnPlane: CartesianVector
  ): Option[CartesianVector] = {
    val t = (planeNormal dot (pointOnPlane - ray.origin)) / (planeNormal dot ray.direction)
    if (t < Epsilon) {
      None
    } else {
      Some(ray.origin + t * ray.direction)
    }
  }

  private def convertFromSolutionUsingShiftedRay(
    ray: CartesianRay,
    shiftBy: CartesianVector
  )(
    f: CartesianRay => Set[SurfacePoint]
  ): Set[SurfacePoint] = {
    val shiftedRay = Ray(origin = ray.origin + shiftBy, direction = ray.direction)
    f(shiftedRay) map { surfacePoint =>
      SurfacePoint(
        point = surfacePoint.point - shiftBy,
        normal = surfacePoint.normal
      )
    }
  }

  private def convertFromSolutionUsingEllipseAlignedToXYPlane(
    ray: CartesianRay,
    ellipse: Ellipse
  )(
    f: (CartesianRay, Ellipse) => Option[CartesianVector]
  ): Option[CartesianVector] = {
      val rotate = perform(ellipse.horizontalTurn * -1, ellipse.verticalTilt * -1)
      val rotatedRay = ray.copy(direction = rotate(ray.direction).toUnitVector)
      val alignedEllipsoid = ellipse.copy(centre = rotate(ellipse.centre))
      f(rotatedRay, alignedEllipsoid) map { undo(ellipse.horizontalTurn * -1, ellipse.verticalTilt * -1) }
  }

  private def convertFromSolutionUsingRayRotatedAroundOrigin(
    ray: CartesianRay,
    horizontalTurn: Radians,
    verticalTilt: Radians
  )(
    f: CartesianRay => Set[SurfacePoint]
  ): Set[SurfacePoint] = {
    val rotate = perform(horizontalTurn, verticalTilt)
    val rotatedRay =
      Ray(
        origin = rotate(ray.origin),
        direction = rotate(ray.direction).toUnitVector
      )
    f(rotatedRay) map { rotatedSurfacePoint =>
      val undoRotation = undo(horizontalTurn, verticalTilt)
      SurfacePoint(
        point = undoRotation(rotatedSurfacePoint.point),
        normal = undoRotation(rotatedSurfacePoint.normal).toUnitVector
      )
    }
  }

  private def convertFromSolutionUsingScaledRay(
    ray: CartesianRay,
    xMultiplier: PositiveNumber,
    yMultiplier: PositiveNumber,
    zMultiplier: PositiveNumber
  )(
    f: CartesianRay => Set[SurfacePoint]
  ): Set[SurfacePoint] = {
    val adjustments = (xMultiplier.value, yMultiplier.value, zMultiplier.value)
    val scaledRay =
      Ray(
        origin = ray.origin transform adjustments,
        direction = (ray.direction transform adjustments).toUnitVector
      )
    f(scaledRay) map { scaledSurfacePoint =>
      SurfacePoint(
        point = scaledSurfacePoint.point transform (1 / xMultiplier, 1 / yMultiplier, 1 / zMultiplier),
        normal = (scaledSurfacePoint.normal transform adjustments).toUnitVector
      )
    }
  }

  private def findIntersectionsWithUnitSphere(
    ray: CartesianRay
  ): Set[SurfacePoint] = {
    val leadingCoefficient = ray.direction dot ray.direction
    val secondTermCoefficient = 2 * (ray.origin dot ray.direction)
    val constant = (ray.origin dot ray.origin) - 1
    quadraticRoots(leadingCoefficient, secondTermCoefficient, constant)
      .filter(_ > Epsilon)
      .map(ray.origin + ray.direction * _)
      .map { point =>
        SurfacePoint(point, point.toUnitVector)
      }
  }

  private def findIntersectionsWithUnitCylinder(
    ray: CartesianRay,
    length: PositiveNumber
  ): Set[SurfacePoint] = {
    val leadingCoefficient = ray.direction.x * ray.direction.x + ray.direction.y * ray.direction.y
    val secondTermCoefficient = 2 * (ray.origin.x * ray.direction.x + ray.origin.y * ray.direction.y)
    val constant = ray.origin.x * ray.origin.x + ray.origin.y * ray.origin.y - 1
    quadraticRoots(leadingCoefficient, secondTermCoefficient, constant)
      .filter(_ > Epsilon)
      .map(ray.origin + ray.direction * _)
      .filter(vector => -1 * length / 2 <= vector.z && vector.z <= length / 2)
      .map(point => SurfacePoint(point, CartesianVector.toUnitVector(point.x, point.y, 0)))
  }

  private def findIntersectionsWithUnitCone(
    ray: CartesianRay,
    height: PositiveNumber
  ): Set[SurfacePoint] = {
    val leadingCoefficient =
      height.value * height.value * (ray.direction.x * ray.direction.x + ray.direction.y * ray.direction.y) - ray.direction.z * ray.direction.z
    val secondTermCoefficient: Double =
      2 * (height.value * height.value * (ray.origin.x * ray.direction.x + ray.origin.y * ray.direction.y) - ray.origin.z * ray.direction.z)
    val constant =
      height.value * height.value * (ray.origin.x * ray.origin.x + ray.origin.y * ray.origin.y) - ray.origin.z * ray.origin.z
    quadraticRoots(leadingCoefficient, secondTermCoefficient, constant)
      .filter(_ > Epsilon)
      .map(ray.origin + ray.direction * _)
      .filter(vector => 0 < vector.z && vector.z <= height)
      .map { point =>
        SurfacePoint(
          point = point,
          normal = CartesianVector.toUnitVector(point.x, point.y, -1 * math.sqrt(point.x * point.x + point.y * point.y) / height.value)
        )
      }
  }

  private def findIntersectionWithUnitEndCap(
    ray: CartesianRay,
    capZPosition: Double
  ): Option[SurfacePoint] = {
    val planeNormal =
      if (capZPosition > 0) CartesianVector.toUnitVector(0, 0 , 1) else CartesianVector.toUnitVector(0, 0 , -1)
    intersectionWithPlane(ray, planeNormal, CartesianVector(0, 0, capZPosition))
      .filter { vector =>
        vector.x * vector.x + vector.y * vector.y <= 1
      }
      .map { point =>
        SurfacePoint(point, planeNormal)
      }
  }

  private def reflectionAtSurface(
    surfacePoint: SurfacePoint,
    incomingDirection: UnitCartesianVector
  ): ReflectedRay =
    ReflectedRay(
      ray = Ray(
        origin = surfacePoint.point,
        direction = (incomingDirection - 2 * (surfacePoint.normal dot incomingDirection) * surfacePoint.normal).toUnitVector
      )
    )

  private def refractionAndReflectionAtSurface(
    surfacePoint: SurfacePoint,
    incomingDirection: UnitCartesianVector,
    shapeRefractiveIndex: RefractiveIndex
  ): (Option[RefractedRay], Option[ReflectedRay]) = {
    val c1 = surfacePoint.normal dot incomingDirection
    val r =
      if (c1 < 0) {
        RefractiveIndex.ofVacuum.value / shapeRefractiveIndex.value
      } else {
        shapeRefractiveIndex.value / RefractiveIndex.ofVacuum.value
      }
    val k = 1 - r * r * (1 - c1 * c1)
    if (k < 0) {
      (
        None,
        Some(
          ReflectedRay(
            Ray(surfacePoint.point, reflectionDirection(surfacePoint.point, surfacePoint.normal, incomingDirection))
          )
        )
      )
    } else {
      val c2 = math.sqrt(1 - r * r * (1 - c1 * c1))
      val refraction =
        Some(
          RefractedRay(
            Ray(surfacePoint.point, (r * incomingDirection + (r * math.abs(c1) - c2) * surfacePoint.normal).toUnitVector)
          )
        )
      val reflection =
        if ((surfacePoint.normal dot incomingDirection) < 0) {
          Some(
            ReflectedRay(
              Ray(surfacePoint.point, reflectionDirection(surfacePoint.point, surfacePoint.normal, incomingDirection))
            )
          )
        } else {
          None
        }
      (refraction, reflection)
    }
  }
  private def reflectionDirection(
    pointOfReflection: CartesianVector,
    surfaceNormal: UnitCartesianVector,
    incomingDirection: UnitCartesianVector
  ): UnitCartesianVector = {
    val generalReflectionVector = incomingDirection - 2 * (surfaceNormal dot incomingDirection) * surfaceNormal
    generalReflectionVector.toUnitVector
  }

  private def refractionMaybe(
    pointOfRefraction: CartesianVector,
    surfaceNormal: UnitCartesianVector,
    incomingDirection: UnitCartesianVector,
    firstRefractiveIndex: RefractiveIndex,
    secondRefractiveIndex: RefractiveIndex
  ): Option[UnitCartesianVector] = {
    val c1 = math.abs(surfaceNormal dot incomingDirection)
    val r = firstRefractiveIndex.value / secondRefractiveIndex.value
    val k = 1 - r * r * (1 - c1 * c1)
    if (k < 0) {
      None
    } else {
      val c2 = math.sqrt(1 - r * r * (1 - c1 * c1))
      Some(
        (r * incomingDirection + (r * c1 - c2) * surfaceNormal).toUnitVector
      )
    }
  }

  private def perform(horizontalTurn: Radians, verticalTilt: Radians): CartesianVector => CartesianVector = { vector =>
    (rotation(Z, horizontalTurn) andThen rotation(Y, verticalTilt)) (vector)
  }

  private def undo(horizontalTurn: Radians, verticalTilt: Radians): CartesianVector => CartesianVector = { vector =>
    (rotation(Y, -1 * verticalTilt) andThen rotation(Z, -1 * horizontalTurn)) (vector)
  }

  private def rotation(axis: UnitCartesianVector, angle: Radians): CartesianVector => CartesianVector = { vector =>
    val W =
      Matrix3D(
        0, -1 * axis.z, axis.y,
        axis.z, 0, -1 * axis.x,
        -1 * axis.y, axis.x, 0
      )
    val rotation = Matrix3D.Identity + (W * sine(angle)) + (W * W * 2 * sine(angle / 2) * sine(angle / 2))
    rotation * vector
  }

  private final case class SurfacePoint(point: CartesianVector, normal: UnitCartesianVector)
}
