package domain

import java.rmi.server.RemoteObjectInvocationHandler

import domain.utils.{AppError, InvalidArgumentError, NonEmptyString}
import task.Task

package object config {

  def readArguments(args: Array[String], defaultDirectory: Directory): Task[Either[AppError, FileConfig]] =
    Task.sync {
      args match {
        case Array(sceneFileString, imageNameString) =>
          toFileConfig(defaultDirectory, sceneFileString, imageNameString)
        case Array(directoryString, sceneFileString, imageNameString) =>
          toFileConfig(directoryString, sceneFileString, imageNameString)
        case _ =>
          Left(InvalidArgumentError(s"Expected three runtime arguments"))
      }
    }

  private def toFileConfig(
    directoryString: String,
    sceneFileString: String,
    imageNameString: String
  ): Either[AppError, FileConfig] =
    for {
      directory <- parseDirectory(directoryString)
      sceneFile <- parseNonEmptyParameter("scene file", sceneFileString)
      imageName <- parseNonEmptyParameter("image name", imageNameString)
    } yield FileConfig(directory, sceneFile, imageName)

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

  private def toFileConfig(
    directory: Directory,
    sceneFileString: String,
    imageNameString: String
  ): Either[AppError, FileConfig] = {
    val fileConfigMaybe = for {
      sceneFile <- NonEmptyString(sceneFileString)
      imageName <- NonEmptyString(imageNameString)
    } yield FileConfig(directory, sceneFile, imageName)
    fileConfigMaybe match {
      case Some(fileConfig) =>
        Right(
          fileConfig
        )
      case None =>
        Left(
          InvalidArgumentError(
            s"Unable to create FileConfig for directory=$directory, sceneFile=$sceneFileString, imageName=$imageNameString"
          )
        )
    }
  }

}
