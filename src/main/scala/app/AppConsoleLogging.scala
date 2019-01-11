package app

import java.time.format.DateTimeFormatter._

import domain.{LoggingAlg, TimeAlg}
import domain.logging.LogLevel
import task.Task

class AppConsoleLogging(
  implicit timeAlg: TimeAlg[Task]
) extends LoggingAlg[Task] {

  override def log(message: String, level: LogLevel): Task[Unit] =
    for {
      now <- timeAlg.now
      _ <- Task.sync(
        println(s"[$level] - ${now.format(ISO_LOCAL_DATE_TIME)} : $message")
      )
    } yield ()
}
