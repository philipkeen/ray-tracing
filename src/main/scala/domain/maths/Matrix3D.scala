package domain.maths

final case class Matrix3D(
  _1_1: Double, _1_2: Double, _1_3: Double,
  _2_1: Double, _2_2: Double, _2_3: Double,
  _3_1: Double, _3_2: Double, _3_3: Double
) { self =>

  def +(that: Matrix3D): Matrix3D =
    Matrix3D(
      _1_1 = self._1_1 + that._1_1, _1_2 = self._1_2 + that._1_2, _1_3 = self._1_3 + that._1_3,
      _2_1 = self._2_1 + that._2_1, _2_2 = self._2_2 + that._2_2, _2_3 = self._2_3 + that._2_3,
      _3_1 = self._3_1 + that._3_1, _3_2 = self._3_2 + that._3_2, _3_3 = self._3_3 + that._3_3
    )

  def *(that: Matrix3D): Matrix3D =
    Matrix3D(
      _1_1 = self._1_1 * that._1_1 + self._1_2 * that._2_1 + self._1_3 * that._3_1,
      _1_2 = self._1_1 * that._1_2 + self._1_2 * that._2_2 + self._1_3 * that._3_2,
      _1_3 = self._1_1 * that._1_3 + self._1_2 * that._2_3 + self._1_3 * that._3_3,
      _2_1 = self._2_1 * that._1_1 + self._2_2 * that._2_1 + self._2_3 * that._3_1,
      _2_2 = self._2_1 * that._1_2 + self._2_2 * that._2_2 + self._2_3 * that._3_2,
      _2_3 = self._2_1 * that._1_3 + self._2_2 * that._2_3 + self._2_3 * that._3_3,
      _3_1 = self._3_1 * that._1_1 + self._3_2 * that._2_1 + self._3_3 * that._3_1,
      _3_2 = self._3_1 * that._1_2 + self._3_2 * that._2_2 + self._3_3 * that._3_2,
      _3_3 = self._3_1 * that._1_3 + self._3_2 * that._2_3 + self._3_3 * that._3_3
    )

  def *(d: Double): Matrix3D =
    Matrix3D(
      _1_1 = self._1_1 * d, _1_2 = self._1_2 * d, _1_3 = self._1_3 * d,
      _2_1 = self._2_1 * d, _2_2 = self._2_2 * d, _2_3 = self._2_3 * d,
      _3_1 = self._3_1 * d, _3_2 = self._3_2 * d, _3_3 = self._3_3 * d
    )

  def *(vector: CartesianVector): CartesianVector =
    CartesianVector(
      x = self._1_1 * vector.x + self._1_2 * vector.y + self._1_3 * vector.z,
      y = self._2_1 * vector.x + self._2_2 * vector.y + self._2_3 * vector.z,
      z = self._3_1 * vector.x + self._3_2 * vector.y + self._3_3 * vector.z
    )
}

object Matrix3D {

  val Identity: Matrix3D =
    Matrix3D(
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    )
}
