package scalox

sealed trait Value {
  def show: String
}

object Value {

  final case class Number(value: Double) extends Value {
    def show = f"${value}%g"

    def add(other: Number)      = copy(value + other.value)
    def subtract(other: Number) = copy(value - other.value)
    def multiply(other: Number) = copy(value * other.value)
    def divide(other: Number)   = copy(value / other.value)
    def negate                  = copy(-value)

    def +(other: Number) = add(other)
    def -(other: Number) = subtract(other)
    def *(other: Number) = multiply(other)
    def /(other: Number) = divide(other)
    def unary_-          = negate
  }
}
