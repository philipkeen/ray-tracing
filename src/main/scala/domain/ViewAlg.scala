package domain

import domain.config.ImageResolution
import domain.maths.CartesianVector
import domain.model._

import scala.math._

object ViewAlg {

  private val PixelCentre = (0.5, 0.5)
  private val PositionsWithinPixel = Vector((0.25, 0.25), (0.75, 0.25), (0.5, 0.5), (0.25, 0.75), (0.75, 0.75))
  private val ScreenSideLength = 2

  private val CameraDirection = CartesianVector.toUnitVector(0, 1, 0)
  private val CameraUp = CartesianVector.toUnitVector(0, 0, 1)
  private val CameraRight = CartesianVector.toUnitVector(1, 0, 0)

  def getCameraRayThroughPixel(pixel: Pixel, resolution: ImageResolution): CartesianRay =
    positionWithinScreen(pixel, PixelCentre, resolution)

//  def getCameraRayThroughPixel(pixel: Pixel, camera: Camera, screen: Screen): CartesianRay = {
//    import camera._, screen._
//
//    screenPositionOfPixelCentre(pixel, screenWidthResolution, screenHeightResolution, pixelLength) match {
//      case (screenX, screenY) =>
//        Ray(
//          origin = camera.location,
//          direction = (camera.viewDirection * distanceFromScreen.value + up * screenY + right * screenX).toUnitVector
//        )
//    }
//  }

  def getCameraRaysThroughPixel(pixel: Pixel, resolution: ImageResolution): Vector[CartesianRay] =
    PositionsWithinPixel map { positionInPixel =>
      positionWithinScreen(pixel, positionInPixel, resolution)
    }

//  def getCameraRaysThroughPixel(pixel: Pixel, camera: Camera, screen: Screen): Vector[CartesianRay] = {
//    import screen._
//
//    screenPositionsWithinPixel(pixel, screenWidthResolution, screenHeightResolution, pixelLength)
//      .map { case (screenX, screenY) =>
//        val toPointOnScreen =
//          camera.viewDirection * camera.distanceFromScreen.value + camera.up * screenY + camera.right * screenX
//        Ray(
//          origin = camera.location,
//          direction = toPointOnScreen.toUnitVector
//        )
//      }
//  }

//  private def screenPositionOfPixelCentre(
//    pixel: Pixel,
//    screenWidth: PositiveInteger,
//    screenHeight: PositiveInteger,
//    pixelLength: PositiveNumber
//  ): (Double, Double) =
//    (
//      (pixel.x + 0.5 - screenWidth.value / 2) * pixelLength.value,
//      (screenHeight.value / 2 - pixel.y - 0.5) * pixelLength.value
//    )
//
//  private def screenPositionsWithinPixel(
//    pixel: Pixel,
//    screenWidth: PositiveInteger,
//    screenHeight: PositiveInteger,
//    pixelLength: PositiveNumber
//  ): Vector[(Double, Double)] =
//    PositionsWithinPixel map { case (deltaX, deltaY) =>
//      (
//        (pixel.x + deltaX - screenWidth.value / 2) * pixelLength.value,
//        (screenHeight.value / 2 - pixel.y - deltaY) * pixelLength.value
//      )
//    }

  private def positionWithinScreen(
    pixel: Pixel,
    positionWithinPixel: (Double, Double),
    resolution: ImageResolution
  ): CartesianRay = {

    // ensure the image fills the imageSettings
    val screenResolution = max(resolution.width.value, resolution.height.value)
    // centre the image - much like the black bars above/below a video when the aspect ratio doesn't match the imageSettings
    val verticalBufferWidth = (screenResolution - resolution.width.value) / 2
    val horizontalBufferWidth = (screenResolution - resolution.height.value) / 2

    val screenX = ScreenSideLength * (verticalBufferWidth + pixel.x + positionWithinPixel._1) / screenResolution - 1
    val screenY = 1 - ScreenSideLength * (horizontalBufferWidth + pixel.y + positionWithinPixel._2) / screenResolution
    Ray(
      origin = CartesianVector.Origin,
      direction = (CameraDirection + CameraUp * screenY + CameraRight * screenX).toUnitVector
    )
  }
}
