package domain

import domain.config.{Directory, RenderSettings}
import domain.error.AppError
import domain.logging.LogLevel
import domain.maths.PositiveInteger
import domain.model._
import domain.parse.FileContents
import domain.utils.NonEmptyString
import task.Task
import task.Task._

final case class App(
  fileIO: FileIO,
  tracingAlg: Tracing,
  loggingAlg: LoggingAlg[Task]
) {
  import loggingAlg._

  def renderScene(
    directory: Directory,
    sceneFile: NonEmptyString,
    picture: Picture
  ): Task[Unit] =
    withSceneLoaded(directory, sceneFile) { fileContents =>
      import fileContents._

      for {
        imageHeight <- pure(picture.imageResolution.height.intValue)
        imageWidth <- pure(picture.imageResolution.width.intValue)
        milestones <- milestones(imageHeight)
        unitOrError <- loop(start = 0, finishAfter = imageHeight - 1) { y =>
          maybeLogProgress(y, imageHeight, milestones) *>
            loop(start = 0, finishAfter = imageWidth - 1) { x =>
              paintPixel(x, y, picture, lightSource, objects, renderSettings)
            }
        }
        _ <- completePicture(picture, unitOrError)
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

  private def maybeLogProgress(y: Int, screenHeight: Int, milestones: Set[Int]): Task[Unit] =
    if (milestones.contains(y)) {
      log(f"${y / screenHeight.toDouble * 100}%3.2f" +"% of picture rendered", LogLevel.Info)
    } else {
      pure(())
    }

  private def loop(
    start: Int,
    finishAfter: Int
  )(
    forEach: Int => Task[Either[AppError, Unit]]
  ): Task[Either[AppError, Unit]] = {
    def runFromIteration(currentValue: Int): Task[Either[AppError, Unit]] =
      forEach(currentValue) flatMap {
        case Right(_) if currentValue < finishAfter => runFromIteration(currentValue + 1)
        case Right(unit) => pure(Right(unit))
        case Left(error) => pure(Left(error))
      }

    runFromIteration(start)
  }

  private def paintPixel(
    x: Int,
    y: Int,
    picture: Picture,
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

  private def completePicture(
    picture: Picture,
    paintingResult: Either[AppError, Unit]
  ): Task[Unit] =
    paintingResult match {
      case Right(_) =>
        log(s"Rendering of ${picture.imageName.value} complete", LogLevel.Info) *> picture.persist
      case Left(error) =>
        log(error.message, LogLevel.Error)
    }
}
