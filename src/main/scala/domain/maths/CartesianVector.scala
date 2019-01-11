package domain.maths

trait CartesianVector extends Vector3D { self =>
  def x: Double
  def y: Double
  def z: Double

  def magnitude: Double = math.sqrt(x * x + y * y + z * z)

  def *(scalar: Double): CartesianVector =
    new ArbitraryCartesianVector(x * scalar, scalar * y, scalar * z) {}

  def transform(by: (Double, Double, Double)): CartesianVector =
    new ArbitraryCartesianVector(by._1 * x, by._2 * y, by._3 * z) {}

  def +(that: CartesianVector): CartesianVector =
    new ArbitraryCartesianVector(x + that.x, y + that.y, z + that.z) {}

  def -(that: CartesianVector): CartesianVector =
    new ArbitraryCartesianVector(x - that.x, y - that.y, z - that.z) {}

  def dot(that: CartesianVector): Double =
    x * that.x + y * that.y + z * that.z

  def cross(that: CartesianVector): CartesianVector =
    new ArbitraryCartesianVector(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x) {}

  def distanceTo(that: CartesianVector): Double =
    (self - that).magnitude

  def toUnitVector: UnitCartesianVector =
    self match {
      case u: UnitCartesianVector => u
      case a: ArbitraryCartesianVector => CartesianVector.toUnitVector(a.x, a.y, a.z)
    }
}

sealed abstract case class ArbitraryCartesianVector private[maths](
  override val x: Double,
  override val y: Double,
  override val z: Double
) extends CartesianVector

sealed abstract case class NonZeroCartesianVector private[maths] (
  override val x: Double,
  override val y: Double,
  override val z: Double
) extends CartesianVector

sealed abstract case class UnitCartesianVector private[maths](
  override val x: Double,
  override val y: Double,
  override val z: Double
) extends CartesianVector with UnitVector

object CartesianVector {

  def apply(x: Double, y: Double, z: Double): CartesianVector = new ArbitraryCartesianVector(x, y, z) {}

  def toUnitVector(x: Double, y: Double, z: Double): UnitCartesianVector = {
    val correction = 1 / math.sqrt(x * x + y * y + z * z)
    new UnitCartesianVector(x * correction, y * correction, z * correction) {}
  }

  val Origin: CartesianVector = new ArbitraryCartesianVector(0, 0, 0) {}
}
