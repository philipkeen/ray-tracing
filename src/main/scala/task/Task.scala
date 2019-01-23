package task

sealed abstract class Task[+A] { self =>
  import Task._

  final def map[B](f: => A => B): Task[B] = FlatMap(self, (a: A) => Pure(f(a)))

  final def flatMap[B](f: => A => Task[B]): Task[B] = FlatMap(self, f)

  final def zip[B](that: => Task[B]): Task[(A, B)] =
    self.flatMap(a => that.map(b => (a, b)))

  final def *> [B](that: => Task[B]): Task[B] =
    self.zip(that).map(_._2)

  final def <* [B](that: => Task[B]): Task[A] =
    self.zip(that).map(_._1)

  final def run(): A =
    self match {
      case Pure(a) => a
      case Sync(call) => call()
      case fm: FlatMap[_, A] => fm.f(fm.source.run()).run()
    }
}

object Task {

  final def pure[A](value: => A): Task[A] = Pure(value)

  final def sync[A](call: => A): Task[A] = Sync(() => call)

  final def traverse[A, B](in: Iterable[A])(f: A => Task[B]): Task[List[B]] =
    in.foldRight[Task[List[B]]](Task.sync(Nil)) { case (a, task) =>
      f(a).zip(task).map { case (b, bs) => b :: bs }
    }

  private[task] final case class Pure[+A](value: A) extends Task[A]

  private[task] final case class Sync[+A](call: () => A) extends Task[A]

  private[task] final case class FlatMap[B, +A](source: Task[B], f: B => Task[A]) extends Task[A]

}
