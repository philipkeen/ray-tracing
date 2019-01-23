package domain.error

import domain.config.ImageResolution
import domain.model.Pixel

trait AppError {
  def message: String

  def combine(appError: AppError): AppError = MultipleErrors(this, appError)
}

final case class ParseError(override val message: String) extends AppError

object ParseError {
  def apply(lineNumber: Int, message: String): ParseError =
    ParseError(s"Error in line $lineNumber: $message")
}

final case class InvalidArgumentError(override val message: String) extends AppError

final case class FileIOError(override val message: String) extends AppError

final case class PixelOutOfRange(pixel: Pixel, imageResolution: ImageResolution) extends AppError {
  import imageResolution._

  override def message: String =
    s"Pixel (${pixel.x}, ${pixel.y}) not in image - x coordinate must lie in [0, ${width.intValue}] and y coordinate " +
      s"must lie in [0, ${height.intValue}]"
}

final case class MultipleErrors(error1: AppError, error2: AppError) extends AppError {
  override def message: String = s"${error1.message}, ${error2.message}"
}
