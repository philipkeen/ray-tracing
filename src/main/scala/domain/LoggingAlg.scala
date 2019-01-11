package domain

import domain.logging.LogLevel

trait LoggingAlg[F[_]] {

  def log(message: String, level: LogLevel): F[Unit]
}
