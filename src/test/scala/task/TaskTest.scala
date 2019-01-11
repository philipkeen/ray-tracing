package task

import org.scalatest.FunSuite

import task.Task._

class TaskTest extends FunSuite {

  test("Task.loop executes the body when run") {
    var sum = 0
    loop[Int](0, i => i + 1, _ == 10) { i =>
      sync(sum += i)
    }.run()
    assert(sum == 55)
  }
}
