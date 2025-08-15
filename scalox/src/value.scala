package scalox

sealed trait Value {
  def show: String
}

object Value {

  final case class Number(value: Double) extends Value {
    def show = f"${value}%g"
  }

}
