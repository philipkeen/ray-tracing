package domain

import domain.config.{Directory, RenderSettings}
import domain.error.AppError
import domain.logging.LogLevel
import domain.model._
import domain.parse.FileContents
import domain.utils.NonEmptyString
import task.Task
import task.Task._

final case class App(
  fileIO: FileIO[Task],
  tracingAlg: Tracing,
  loggingAlg: LoggingAlg[Task]
) {
  import loggingAlg._

  def renderScene(
    directory: Directory,
    sceneFile: NonEmptyString,
    picture: Picture[Task]
  ): Task[Unit] =
    withSceneLoaded(directory, sceneFile) { fileContents =>
      import fileContents._

      for {
        imageHeight <- pure(picture.imageResolution.height.intValue)
        imageWidth <- pure(picture.imageResolution.width.intValue)
        milestones <- milestones(imageHeight)
        paintedOrError <- loop((0 until imageHeight).toList) { y =>
          loop((0 until imageWidth).toList) { x =>
            paintPixel(x, y, picture, lightSource, objects, renderSettings) <*
              maybeLogProgress(x, y, imageHeight, milestones)
          }
        }
        _ <- persistPicture(picture, paintedOrError)
      } yield ()
    }

  private def withSceneLoaded(
    directory: Directory,
    sceneFile: NonEmptyString
  )(
    f: FileContents => Task[Unit]
  ): Task[Unit] =
    fileIO.loadPictureSettingsFromFile(directory, sceneFile) flatMap {
      case Right(fileContents) => f(fileContents)
      case Left(error) => log(error.message, LogLevel.Error)
    }

  private def milestones(height: Int): Task[Set[Int]] = {
    val step = height / 10
    pure(
      if (step > 0) (0 to height by step).toSet else Set()
    )
  }

  private def maybeLogProgress(x: Int, y: Int, screenHeight: Int, milestones: Set[Int]): Task[Unit] =
    if (x == 0 && milestones.contains(y)) {
      log(f"${(screenHeight - y) / screenHeight.toDouble * 100}%3.2f" +"% of picture rendered", LogLevel.Info)
    } else {
      pure(())
    }

  private def loop(
    ints: List[Int]
  )(
    forEach: Int => Task[Either[AppError, Unit]]
  ): Task[Either[AppError, Unit]] =
    traverseOrError(ints)(forEach).map{
      listOrError => listOrError.map(_ => ())
    }

  private def paintPixel(
    x: Int,
    y: Int,
    picture: Picture[Task],
    lightSource: LightSource,
    objects: Set[Shape],
    renderSettings: RenderSettings
  ): Task[Either[AppError, Unit]] =
    for {
      pixel <- pure(Pixel(x, y))
      pixelRays <- sync { ViewAlg.getCameraRaysThroughPixel(pixel, picture.imageResolution).par }
      coloursWithinPixel <- sync { pixelRays.map(tracingAlg.castRay(_, lightSource, objects, renderSettings)) }
      pixelColour <- pure(Colour.average(coloursWithinPixel.toList))
      unitOrError <- picture.paintPixel(pixel, pixelColour)
    } yield unitOrError

  private def persistPicture(
    picture: Picture[Task],
    paintingResult: Either[AppError, Unit]
  ): Task[Unit] =
    paintingResult match {
      case Right(_) =>
        log(s"Rendering of ${picture.imageName.value} complete", LogLevel.Info) *> picture.persist
      case Left(error) =>
        log(error.message, LogLevel.Error)
    }
}
