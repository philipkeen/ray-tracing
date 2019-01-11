import app.{AppConsoleLogging, AppRandom, AppTimer}
import domain.{App, Tracing}
import domain.config._
import domain.logging.LogLevel
import domain.utils.NonEmptyString
import task.Task

object Main {

  def main(args: Array[String]): Unit = {
    implicit val timeAlg: AppTimer = new AppTimer()

    val tracingAlg = Tracing(new AppRandom())
    val loggingAlg = new AppConsoleLogging()
    val app = App(tracingAlg, loggingAlg)

    val renderTask = for {
      fileConfigOrError <- readArguments(args, Directory(System.getProperty("user.home") + "/").get)
      renderAttempt <- fileConfigOrError match {
        case Right(fc) => app.renderScene(fc.directory, fc.sceneFile, fc.imageName)
        case Left(error) => loggingAlg.log(s"$error", LogLevel.Error) *> Task.pure(())
      }
    } yield renderAttempt
    renderTask.run()
  }

}
