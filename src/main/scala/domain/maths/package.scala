package domain

package object maths {

  def sine(angle: Radians): Double = math.sin(angle.value)

  def cosine(angle: Radians): Double = math.cos(angle.value)

  def inverseTangent(t: Double): Radians =
    Radians(math.atan(t))

  def inverseCosine(c: Double): Option[Radians] =
    if (math.abs(c) <= 1) {
      Some(Radians(math.acos(c)))
    } else {
      None
    }

  // Returns values for x for which ax^2 + bx + c = 0
  def quadraticRoots(a: Double, b: Double, c: Double): Set[Double] =
    if (b * b - 4 * a * c < 0) {
      Set.empty
    } else {
      Set(
        (-b + math.sqrt(b * b - 4 * a * c)) / (2 * a),
        (-b - math.sqrt(b * b - 4 * a * c)) / (2 * a)
      )
    }
}
