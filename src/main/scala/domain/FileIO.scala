package domain

import domain.config.Directory
import domain.error.AppError
import domain.parse._
import domain.utils.NonEmptyString

trait FileIO[F[_]] {
  def loadPictureSettingsFromFile(directory: Directory, file: NonEmptyString): F[Either[AppError, FileContents]]
}
