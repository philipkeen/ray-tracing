package domain.logging

trait LogLevel

object LogLevel {

  case object Info extends LogLevel
  case object Error extends LogLevel
}
