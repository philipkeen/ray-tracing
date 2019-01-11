package app

import java.time.ZonedDateTime

import domain.Timer
import task.Task

class AppTimer extends Timer[Task] {
  override def now: Task[ZonedDateTime] = Task.pure(ZonedDateTime.now())
}
