package domain

import java.awt.image.BufferedImage

import domain.config.{Directory, ImageResolution}
import domain.logging.LogLevel
import domain.maths.PositiveInteger
import domain.model._
import domain.parse.FileContents
import domain.utils.NonEmptyString
import task.Task
import task.Task._

final case class App(
                      tracingAlg: Tracing,
                      loggingAlg: LoggingAlg[Task]
) {
  import loggingAlg._

  def renderScene(
    directory: Directory,
    sceneFile: NonEmptyString,
    imageName: NonEmptyString
  ): Task[Unit] =
    withSceneLoaded(directory, sceneFile) { fileContents =>
      import fileContents._

      for {
        image <- createImage(imageResolution)
        graphics <- sync(image.createGraphics())
        milestones <- milestones(imageResolution)
        _ <- loop(0, incrementBy(1), finishAfter(imageResolution.height.intValue - 1)) { y =>
          maybeLogProgress(y, imageResolution.height, milestones) *>
            loop(0, incrementBy(1), finishAfter(imageResolution.width.intValue - 1)) { x =>
              sync {
                val pixel = Pixel(x, y)
                val pixelRays = ViewAlg.getCameraRaysThroughPixel(pixel, imageResolution).par
                val coloursWithinPixel = pixelRays.map(tracingAlg.castRay(_, lightSource, objects, renderSettings))
                val pixelColour = Colour.average(coloursWithinPixel.toList)
                graphics.setColor(pixelColour.toJavaColor)
                graphics.fillRect(pixel.x, pixel.y, 1, 1)
              }
            }
        }
        _ <- sync(graphics.dispose())
        _ <- FileIO.savePicture(image, directory, imageName)
        _ <- log(s"Rendering of ${imageName.value} complete", LogLevel.Info)
      } yield ()
    }

  private def withSceneLoaded(
    directory: Directory,
    sceneFile: NonEmptyString
  )(
    f: FileContents => Task[Unit]
  ): Task[Unit] =
    FileIO.loadPictureSettingsFromFile(directory, sceneFile) flatMap {
      case Right(fileContents) => f(fileContents)
      case Left(error) => log(error.message, LogLevel.Error)
    }

  private def createImage(resolution: ImageResolution): Task[BufferedImage] =
    pure(
      new BufferedImage(
        resolution.width.intValue,
        resolution.height.intValue,
        BufferedImage.TYPE_INT_RGB
      )
    )

  private def milestones(resolution: ImageResolution): Task[Set[Int]] = {
    val step = resolution.height.intValue / 10
    pure(
      if (step > 0) (0 to resolution.height.intValue by step).toSet else Set()
    )
  }

  private def maybeLogProgress(y: Int, screenHeight: PositiveInteger, milestones: Set[Int]): Task[Unit] =
    if (milestones.contains(y)) {
      log(f"${y / screenHeight.value * 100}%3.2f" +"% of picture rendered", LogLevel.Info)
    } else {
      pure(())
    }

  private def incrementBy(i : Int): Int => Int = _ + i

  private def finishAfter(max: Int): Int => Boolean = _ >= max
}
