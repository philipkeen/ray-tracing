package domain

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import domain.config.Directory
import domain.parse._
import domain.utils.{AppError, FileIOError, NonEmptyString}
import task.Task

import scala.util.{Failure, Success, Try}

object FileIO {
  def loadPictureSettingsFromFile(directory: Directory, file: NonEmptyString): Task[Either[AppError, FileContents]] =
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

  def savePicture(image: BufferedImage, directory: Directory, imageName: NonEmptyString): Task[Unit] =
    Task.sync({
      ImageIO.write(image, "jpg", new File(directory.value + "/" + imageName.value + ".jpg"))
    })
}
