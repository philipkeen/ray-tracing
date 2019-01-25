package app

import domain.FileIO
import domain.config.Directory
import domain.error.{AppError, FileIOError}
import domain.parse.{FileContents, parseFileContents}
import domain.utils.NonEmptyString
import task.Task

import scala.util.{Failure, Success, Try}

class AppFileIO extends FileIO[Task] {

  override def loadPictureSettingsFromFile(
    directory: Directory,
    file: NonEmptyString
  ): Task[Either[AppError, FileContents]] =
    Task.sync({
      Try(io.Source.fromFile(directory.value + "/" + file.value)) match {
        case Success(fileSource) =>
          val sceneOrError = parseFileContents(fileSource.getLines.toList)
          fileSource.close()
          sceneOrError
        case Failure(error) =>
          Left(FileIOError(s"Error opening ${file.value}:\n $error"))
      }
    })
}
