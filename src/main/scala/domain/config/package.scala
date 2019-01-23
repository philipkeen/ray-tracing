package domain

import domain.error.{AppError, InvalidArgumentError}
import domain.maths.PositiveInteger
import domain.utils.NonEmptyString
import task.Task

package object config {

  private val positiveIntegerRegex = """^[1-9]\d*$""".r

  def readPictureConfig(args: Array[String], defaultDirectory: Directory): Task[Either[AppError, PictureConfig]] =
    Task.sync {
      args match {
        case Array(sceneFileString, imageNameString, widthString, heightString) =>
          toPictureConfig(defaultDirectory, sceneFileString, imageNameString, widthString, heightString)
        case Array(directoryString, sceneFileString, imageNameString, widthString, heightString) =>
          toPictureConfig(directoryString, sceneFileString, imageNameString, widthString, heightString)
        case _ =>
          Left(InvalidArgumentError(s"Expected runtime arguments to file configuration"))
      }
    }

  private def toPictureConfig(
    directoryString: String,
    sceneFileString: String,
    imageNameString: String,
    widthString: String,
    heightString: String
  ): Either[AppError, PictureConfig] =
    for {
      directory <- parseDirectory(directoryString)
      sceneFile <- parseNonEmptyParameter("scene file", sceneFileString)
      imageName <- parseNonEmptyParameter("image name", imageNameString)
      width <- parsePositiveInteger("width", widthString)
      height <- parsePositiveInteger("height", heightString)
    } yield PictureConfig(directory, sceneFile, imageName, ImageResolution(width, height))

  private def toPictureConfig(
    directory: Directory,
    sceneFileString: String,
    imageNameString: String,
    widthString: String,
    heightString: String
  ): Either[AppError, PictureConfig] =
    for {
      sceneFile <- parseNonEmptyParameter("scene file", sceneFileString)
      imageName <- parseNonEmptyParameter("image name", imageNameString)
      width <- parsePositiveInteger("width", widthString)
      height <- parsePositiveInteger("height", heightString)
    } yield PictureConfig(directory, sceneFile, imageName, ImageResolution(width, height))

  private def parseDirectory(directoryString: String): Either[AppError, Directory] =
    Directory(directoryString) match {
      case Some(directory) => Right(directory)
      case None => Left(InvalidArgumentError("A directory must be a non-empty string ending with '/'"))
    }

  private def parseNonEmptyParameter(parameterName: String, parameterString: String): Either[AppError, NonEmptyString] =
    NonEmptyString(parameterString) match {
      case Some(nonEmptyString) => Right(nonEmptyString)
      case None => Left(InvalidArgumentError(s"The $parameterName must be a non-empty string"))
    }

  private def parsePositiveInteger(parameterName: String, parameterString: String): Either[AppError, PositiveInteger] =
    positiveIntegerRegex.findFirstIn(parameterString) match {
      case Some(positiveInteger) => PositiveInteger(positiveInteger.toInt)
      case None => Left(InvalidArgumentError(s"The $parameterName must be a positive integer, received $parameterString"))
    }

}
