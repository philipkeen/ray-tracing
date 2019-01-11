import app.{AppConsoleLogging, AppRandom, AppTime}
import domain.{App, TracingAlg}
import domain.config._
import domain.logging.LogLevel
import domain.utils.NonEmptyString
import task.Task

object Main {

  def main(args: Array[String]): Unit = {
    implicit val timeAlg: AppTime = new AppTime()

    val tracingAlg = TracingAlg(new AppRandom())
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


//    println("-----------------------------------------------------------------------------")
//
//    (5 to 14).toList.foreach { y =>
//      (-5 to 4).toList.foreach { x =>
//        val X = x * 100
//        val Y = y * 100
//        println("triangle")
//        println(s"  vertex0: ($X, ${Y + 100}, -125)")
//        println(s"  vertex1: ($X, $Y, -125)")
//        println(s"  vertex2: (${X + 100}, $Y, -125)")
//        println("  reflectivity: 0")
//        println("  transparency: 0")
//        if ((x + y) % 2 != 0) {
//          println("  colour: ffffff")
//        } else {
//          println("  colour: 777777")
//        }
//        println("triangle")
//        println(s"  vertex0: ($X, ${Y + 100}, -125)")
//        println(s"  vertex1: (${X + 100}, ${Y + 100}, -125)")
//        println(s"  vertex2: (${X + 100}, $Y, -125)")
//        println("  reflectivity: 0")
//        println("  transparency: 0")
//        if ((x + y) % 2 != 0) {
//          println("  colour: ffffff")
//        } else {
//          println("  colour: 777777")
//        }
//      }
//    }

  }

}
