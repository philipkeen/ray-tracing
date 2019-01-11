package domain.utils

trait AppError {
  def message: String
}

final case class ParseError(override val message: String) extends AppError

object ParseError {
  def apply(lineNumber: Int, message: String): ParseError =
    ParseError(s"Error in line $lineNumber: $message")
}

final case class InvalidArgumentError(override val message: String) extends AppError

final case class FileIOError(override val message: String) extends AppError
