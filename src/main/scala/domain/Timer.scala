package domain

import java.time.ZonedDateTime

trait Timer[F[_]] {

  def now: F[ZonedDateTime]
}
