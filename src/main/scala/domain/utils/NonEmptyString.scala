package domain.utils

sealed abstract case class NonEmptyString private (value: String)

object NonEmptyString {
  def apply(value: String): Option[NonEmptyString] =
    if (value.nonEmpty) {
      Some(new NonEmptyString(value) {})
    } else {
      None
    }
}
