package domain.model

import org.scalatest.FunSuite

class ColourTest extends FunSuite {

  test("Colour.average correctly updates blue") {
    val blue = Colour(0, 45, 120).right.get

    assert((blue * (1.0 / 3)).red.value == 0)
    assert((blue * (1.0 / 3)).green.value == 15)
    assert((blue * (1.0 / 3)).blue.value == 40)
  }
}
