import app._
import domain.{App, Tracing}
import domain.config._
import domain.logging.LogLevel
import task.Task._

object Main {

  def main(args: Array[String]): Unit = {
    implicit val timeAlg: AppTimer = new AppTimer()

    val fileIO = new AppFileIO()
    val tracingAlg = Tracing(new AppRandom())
    val loggingAlg = new AppConsoleLogging()
    val app = App(fileIO, tracingAlg, loggingAlg)

    val renderTask = for {
      pictureConfigOrError <- readPictureConfig(args, Directory(System.getProperty("user.home") + "/").get)
      renderAttempt <- pictureConfigOrError match {
        case Right(pictureConfig) =>
          import pictureConfig._
          for {
            picture <- pure(new AppPicture(directory, imageName, imageResolution))
            _ <- app.renderScene(directory, sceneFile, picture)
          } yield ()
        case Left(error) => loggingAlg.log(s"$error", LogLevel.Error) *> pure(())
      }
    } yield renderAttempt
    renderTask.run()
  }

}
