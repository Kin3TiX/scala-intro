class Rational(x: Int, y: Int) {
  require(y > 0, "denominator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if(b == 0) a else gcd(b, a%b)

  private val g = gcd(x, y)

  val numerator: Int = x
  val denominator: Int = y
  lazy val simple_numerator: Int = x/gcd(x, y)
  lazy val simple_denominator: Int = y/gcd(x, y)

  def unary_- : Rational =
    new Rational(-this.numerator, this.denominator)

  def + (that: Rational) =
    new Rational(
      this.numerator*that.denominator + that.numerator*this.denominator,
      this.denominator*that.denominator
    )

  def - (that: Rational) =
    this + -that

  def < (that: Rational) =
    this.numerator*that.denominator < that.numerator*this.denominator

  def max (that:Rational) =
    if(this < that) that else this

  override def toString: String =
    this.simple_numerator + "/" + this.simple_denominator

}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.numerator
x.denominator
x + y
x - y - z
y + y
x < y
x.max(y)
val simple = new Rational(6)