package domain

import java.time.ZonedDateTime

trait TimeAlg[F[_]] {

  def now: F[ZonedDateTime]
}
