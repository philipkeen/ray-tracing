package domain

import domain.config.RenderSettings
import domain.instances._
import domain.maths._
import domain.model._
import domain.maths.PositiveNumber._

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

final case class TracingAlg(random: RandomAlg) {

  def castRay(
    ray: CartesianRay,
//    lightSource: LightSource,
//    objects: Set[Shape],
//    renderSettings: RenderSettings
//  ): Colour = {
//    rayPath(ray, lightSource, objects, renderSettings, reflectionCount = 0, acc = List())
//      .foldLeft(Option.empty[Colour]) {
//        case (Some(reflectedColour), ColouredPoint(colour, reflectivity, intensity)) =>
//          Some(
//            reflectedColour * reflectivity + colour * (1 - reflectivity) * intensity
//          )
//        case (None, ColouredPoint(colour, _, intensity)) =>
//          Some(
//            colour * intensity
//          )
//      }.getOrElse(renderSettings.defaultColour)
//  }
//
//  def traceRay(
//    initialRay: CartesianRay,
    lightSource: LightSource,
    objects: Set[Shape],
    renderSettings: RenderSettings
  ): Colour = {
    val maxIterations = renderSettings.maxReflections
    val maxShadowRays = renderSettings.maxShadowRaysPerPoint

    def tracePaths(
      rayForThisIteration: CartesianRay,
      iterationCount: Int
    ): Colour =
      rayHit(rayForThisIteration, objects) match {
        case Some((shape, hit)) =>
          shape match {
            case solid: SolidShape if iterationCount < maxIterations =>
              withPossibleReflection(hit.reflectionRay, solid, iterationCount) {
                withPossibleRefraction(hit.refractionRay, solid, iterationCount) {
                  colourInShade(hit.intersectionPoint, hit.surfaceNormal, solid.colour, lightSource, objects, maxShadowRays)
//                  solid.colour * castShadow(hit, lightSource, objects, maxShadowRays)
                }
              }
            case _ if iterationCount < maxIterations =>
              withPossibleReflection(hit.reflectionRay, shape, iterationCount) {
                colourInShade(hit.intersectionPoint, hit.surfaceNormal, shape.colour, lightSource, objects, maxShadowRays)
//                shape.colour * castShadow(hit, lightSource, objects, maxShadowRays)
              }
            case _ =>
              colourInShade(hit.intersectionPoint, hit.surfaceNormal, shape.colour, lightSource, objects, maxShadowRays)
//              shape.colour * castShadow(hit, lightSource, objects, maxShadowRays)
          }
        case None =>
          renderSettings.defaultColour
      }

    def withPossibleReflection(reflectionMaybe: Option[ReflectedRay], shape: Shape, iteration: Int): Colour => Colour =
    { colour =>
      reflectionMaybe match {
        case Some(reflection) if shape.reflectivity.value > 0 =>
          tracePaths(reflection.ray, iteration + 1) * shape.reflectivity.value + colour * (1 - shape.reflectivity.value)
        case _ =>
          colour
      }
    }

    def withPossibleRefraction(refractionMaybe: Option[RefractedRay], solid: SolidShape, iteration: Int): Colour => Colour =
    { colour =>
      refractionMaybe match {
        case Some(refraction) if solid.transparency.value > 0 =>
          tracePaths(refraction.ray, iteration + 1) * solid.transparency.value + colour * (1 - solid.transparency.value)
        case _ =>
          colour
      }
    }

    tracePaths(ray, iterationCount = 0)
  }

  private def rayHit(ray: CartesianRay, shapes: Set[Shape]): Option[(Shape, Hit)] =
    shapes
      .foldLeft(None: Option[(Shape, Hit)]) { case (current, shape) =>
        PhysicsAlg.contact(ray, shape) match {
          case Some(hit) if current.forall(_._2.distanceFromRayOrigin > hit.distanceFromRayOrigin) =>
            Some((shape, hit))
          case _ => current
        }
      }

  private def castShadow(
    hit: Hit,
    lightSource: LightSource,
    objects: Set[Shape],
    rayCount: PositiveInteger
  ): Double = {
    val toLightSource = getRaysToLightSource(hit.intersectionPoint, lightSource, rayCount)
    val intensities =
      toLightSource map { ray =>
        if (objects.exists(shape => PhysicsAlg.contact(ray, shape).nonEmpty)) {
          0.2
        } else {
          val cosAngle = hit.surfaceNormal dot ray.direction
          0.2 + 0.8 * math.abs(cosAngle)
        }
      }
    intensities.sum/intensities.size
  }

  private def colourInShade(
    point: CartesianVector,
    surfaceNormal: UnitCartesianVector,
    baseColour: Colour,
    lightSource: LightSource,
    objects: Set[Shape],
    rayCount: PositiveInteger
  ): Colour = {
    val ambience = lightSource.ambience.value
    val toLightSource = getRaysToLightSource(point, lightSource, rayCount)
    val colours =
      toLightSource map { ray =>
        val cosAngle = surfaceNormal dot ray.direction
        val inRaysPath = shapesInRaysPath(ray, objects)
        if (inRaysPath.nonEmpty) {
          if (throwsAShade(inRaysPath)) {
            baseColour * ambience
          } else {
            combinedFilteredColour(
              baseColour = baseColour,
              objects = shapesInRaysPath(ray, objects),
              acc = baseColour
            ) * (ambience + (1 - ambience) * math.abs(cosAngle))
          }
        } else {
          baseColour * (ambience + (1 - ambience) * math.abs(cosAngle))
        }
      }
    Colour.average(colours.toList)
  }

  private def shapesInRaysPath(
    ray: CartesianRay,
    objects: Set[Shape]
  ): List[Shape] =
    objects
      .map(shape => PhysicsAlg.contact(ray, shape).map(hit => (shape, hit)))
      .flatMap(_.map { case (shape, hit) => (shape, hit.intersectionPoint) })
      .toList
      .sortBy(_._2 distanceTo ray.origin)
      .map(_._1)

  private def throwsAShade(objects: List[Shape]): Boolean =
    objects.nonEmpty && objects.exists(_.transparency.value == 0)

  @tailrec
  private def combinedFilteredColour(baseColour: Colour, objects: List[Shape], acc: Colour): Colour =
    objects match {
      case Nil =>
        acc
      case transparentShape :: remainingShapes =>
        combinedFilteredColour(
          baseColour = baseColour,
          objects = remainingShapes,
          acc = transparentShape.colour * (1 - transparentShape.transparency.value) + acc * transparentShape.transparency.value
        )
    }

  private def getRaysToLightSource(
    point: CartesianVector,
    lightSource: LightSource,
    rayCount: PositiveInteger
  ): ParVector[CartesianRay] = {
    val targetPoints =
      if (rayCount.intValue == 1 || lightSource.radius == NonNegativeNumber.Zero) {
        Vector(lightSource.location)
      } else {
        Vector.fill(rayCount.intValue) {
          SphericalCoordinateVector(
            random.randomNonNegativeDouble(lightSource.radius),
            Radians(random.randomNonNegativeDouble(Pi + Pi)),
            Radians(random.randomNonNegativeDouble(Pi))
          ).toCartesianVector + lightSource.location
        }
      }
    targetPoints
      .map { p =>
        Ray(point, (p - point).toUnitVector)
      }.par
  }

  private final case class ColouredPoint(colour: Colour, reflectivity: Proportion, intensity: Double)
}
