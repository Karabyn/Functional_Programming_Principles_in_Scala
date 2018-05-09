object Rationals {
  val x = new Rational(1, 3)
  x.numer
  x.denom

  val y = new Rational(5, 7)
  println(x.add(y).toString)

  val z = new Rational(3, 2)

  x.sub(y).sub(z)
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString: String = numer + "/" + denom
}