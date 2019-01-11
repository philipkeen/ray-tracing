package app

import java.time.ZonedDateTime

import domain.TimeAlg
import task.Task

class AppTime extends TimeAlg[Task] {
  override def now: Task[ZonedDateTime] = Task.pure(ZonedDateTime.now())
}
