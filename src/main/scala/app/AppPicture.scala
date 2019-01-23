package app

import java.awt.image.BufferedImage
import java.io.File

import domain.Picture
import domain.config.{Directory, ImageResolution}
import domain.error.{AppError, PixelOutOfRange}
import domain.model.{Colour, Pixel}
import domain.utils.NonEmptyString
import javax.imageio.ImageIO
import task.Task
import task.Task._

class AppPicture(
  directory: Directory,
  override val imageName: NonEmptyString,
  override val imageResolution: ImageResolution
) extends Picture {

  private val ImagePixelSideLength = 1

  private lazy val bufferedImage =
    new BufferedImage(
      imageResolution.width.intValue,
      imageResolution.height.intValue,
      BufferedImage.TYPE_INT_RGB
    )
  private lazy val graphics = bufferedImage.createGraphics()

  override def paintPixel(pixel: Pixel, colour: Colour): Task[Either[AppError, Unit]] =
    if (isPixelInImage(pixel)) {
      for {
        _ <- sync { graphics.setColor(colour.toJavaColor) }
        _ <- sync { graphics.fillRect(pixel.x, pixel.y, ImagePixelSideLength, ImagePixelSideLength)}
      } yield Right(())
    } else {
      Task.pure(Left(PixelOutOfRange(pixel, imageResolution)))
    }

  private def isPixelInImage(pixel: Pixel): Boolean =
    pixel.x >= 0 && pixel.y >= 0 && pixel.x < imageResolution.width.intValue && pixel.y < imageResolution.height.intValue

  override def persist: Task[Unit] =
    for {
      _ <- sync(graphics.dispose())
      _ <- Task.sync {
        ImageIO.write(bufferedImage, "jpg", new File(directory.value + "/" + imageName.value + ".jpg"))
      }
    } yield ()
}
