package domain

import domain.config.Directory
import domain.error.AppError
import domain.parse._
import domain.utils.NonEmptyString
import task.Task

trait FileIO {
  def loadPictureSettingsFromFile(directory: Directory, file: NonEmptyString): Task[Either[AppError, FileContents]]
}
