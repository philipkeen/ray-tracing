package domain.maths

import domain.utils.InvalidArgumentError

trait RealNumber { self =>

  def value: Double

  def <(that: RealNumber): Boolean = self.value < that.value

  def <(double: Double): Boolean = self.value < double

  def <=(that: RealNumber): Boolean = self.value <= that.value

  def <=(double: Double): Boolean = self.value <= double

  def >(that: RealNumber): Boolean = self.value > that.value

  def >(double: Double): Boolean = self.value > double

  def >=(that: RealNumber): Boolean = self.value >= that.value

  def >=(double: Double): Boolean = self.value >= double

  def *(d: Double): RealNumber = new RealNumber {
    override def value: Double = self.value * d
  }

  def +(that: RealNumber): RealNumber =
    new RealNumber {
      override def value: Double = self.value + that.value
    }

  def -(that: RealNumber): RealNumber = new RealNumber {
    override def value: Double = self.value - that.value
  }

  def *(that: RealNumber): RealNumber =
    new RealNumber {
      override def value: Double = self.value * that.value
    }

  def /(that: RealNumber): Option[RealNumber] =
    if (that.value != 0) {
      Some(new RealNumber {
        override def value: Double = self.value / that.value
      })
    } else {
      None
    }

  override def toString: String = value.toString
}

sealed abstract class NonNegativeNumber extends RealNumber { self =>

  def +(n: NonNegativeNumber): NonNegativeNumber =
    new NonNegativeNumber() {
      override val value: Double = self.value + n.value
    }

  def *(n: NonNegativeNumber): NonNegativeNumber =
    new NonNegativeNumber() {
      override val value: Double = self.value * n.value
    }

  def remainder(divisor: PositiveNumber): NonNegativeNumber =
    new NonNegativeNumber {
      override val value: Double = self.value % divisor.value
    }
}

sealed abstract class PositiveNumber extends NonNegativeNumber { self =>

  def +(p: PositiveNumber): PositiveNumber =
    new PositiveNumber {
      override val value: Double = self.value + p.value
    }

  def *(p: PositiveNumber): PositiveNumber =
    new PositiveNumber {
      override val value: Double = self.value * p.value
    }

  def /(p: PositiveNumber): PositiveNumber =
    new PositiveNumber {
      override val value: Double = self.value / p.value
    }

  def /(d: Double): Double =
    self.value / d

  def ^(d: Double): PositiveNumber =
    new PositiveNumber {
      override def value: Double = math.pow(self.value, d)
    }
}

sealed abstract class PositiveInteger extends PositiveNumber {
   def intValue: Int = value.toInt
}

object RealNumber {
  def apply(d: Double): RealNumber = new RealNumber {
    override def value: Double = d
  }
}

object NonNegativeNumber {
  def apply(d: Double): Either[InvalidArgumentError, NonNegativeNumber] =
    if (d >= 0) {
      Right(new NonNegativeNumber { override val value: Double = d })
    } else {
      Left(InvalidArgumentError(s"Unable to create NonNegativeNumber: $d < 0"))
    }

  def absoluteValue(d: Double): NonNegativeNumber =
    new NonNegativeNumber() { override val value: Double = math.abs(d) }

  def square(d: Double): NonNegativeNumber =
    new NonNegativeNumber() { override val value: Double = d * d }

  def square[N <: NonNegativeNumber](n: N): NonNegativeNumber =
    new NonNegativeNumber() { override val value: Double = n.value * n.value }

  def square(p: PositiveNumber): PositiveNumber =
    new PositiveNumber() { override val value: Double = p.value * p.value }

  def nonNegativeSqrt[N <: NonNegativeNumber](n: N): NonNegativeNumber =
    new NonNegativeNumber() { override val value: Double = math.sqrt(n.value) }

  val Zero: NonNegativeNumber = new NonNegativeNumber() { override val value: Double = 0 }

  implicit val nonNegativeNumberOrdering: Ordering[NonNegativeNumber] = Ordering.by(_.value)
}

object PositiveNumber {
  def apply(d: Double): Either[InvalidArgumentError, PositiveNumber] =
    if (d > 0) {
      Right(new PositiveNumber() { override val value: Double = d })
    } else {
      Left(InvalidArgumentError(s"Unable to create PositiveNumber: $d <= 0"))
    }

  val Pi: PositiveNumber = new PositiveNumber() { override val value: Double = math.Pi }
}

object PositiveInteger {
  def apply(i: Int): Either[InvalidArgumentError, PositiveInteger] =
    if (i > 0) {
      Right(new PositiveInteger { override def value: Double = i.toDouble })
    } else {
      Left(InvalidArgumentError(s"Unable to create PositiveInteger: $i <= 0"))
    }

  val One: PositiveInteger = new PositiveInteger() { override val value: Double = 1 }
  val Ten: PositiveInteger = new PositiveInteger() { override val value: Double = 10 }
}
