package svlis.lib

import scala.math._

class Interval(val lo: Double, val hi: Double) {
  assert(hi >= lo, "Interval lo must be below hi")

  override def toString(): String = f"Interval [$lo%.3f, $hi%.3f]"

  final override def equals (other: Any): Boolean = {
    val that = other.asInstanceOf[Interval]
    if (that == null) false
    else (lo == that.lo && hi == that.hi)
  }
  final override def hashCode = 13 * lo.hashCode + 17 * hi.hashCode

  // Membership test value represented by an interval
  def member(): MemTest.Value = {
    if (lo > 0.0)
      MemTest.Air
    else if (hi >= 0.0)
      MemTest.Surface
    else
      MemTest.Solid
  }

  // Membership test of a value in an interval
  def member(v: Double): MemTest.Value = {
    if (v == lo || v == hi)
      MemTest.Surface
    else if (v >= lo && v <= hi)
      MemTest.Solid
    else
      MemTest.Air
  }

  // Monadic minus
  def unary_-(): Interval = new Interval(-hi, -lo)

  // Intervals and reals

  def +(b: Double): Interval = new Interval(lo + b, hi + b)

  def -(b: Double): Interval = this + -b

  def *(b: Double): Interval = {
    if (b > 0.0)
      new Interval(lo * b, hi * b)
    else
      new Interval(hi * b, lo * b)
  }

  def /(b: Double): Interval = {
    assert(b != 0.0)
    // TODO svlis_error("sv_interval::operator/","division by 0", SV_WARNING)
    this * (1 / b)
  }

  // Interval arithmetic

  def +(b: Interval): Interval = new Interval(lo + b.lo, hi + b.hi)

  def -(b: Interval): Interval = new Interval(lo - b.hi, hi - b.lo)

  def *(b: Interval): Interval = {
    val c = lo * b.lo
    val d = c
    val q = lo * b.hi
    val r = hi * b.lo
    val s = hi * b.hi

    val l = min(min(c, q), min(r, s))
    val h = max(max(d, q), max(r, s))
    new Interval(l, h)
  }

  // Interval division is not defined, as Svlis does not support rationals (yet...)

  // Intervals the same?
  // same

  // Absolute value of an interval
  // abs

  // Sign of an interval
  // sign

  // max

  // min

  // Raise an interval to a power
  final def pow(i: Double): Interval = {
    assert(i >= 0)
    // TODO svlis_error("sv_interval::pow","negative exponent",SV_WARNING)
    if (i % 2 != 0) {
      new Interval(scala.math.pow(lo, i), scala.math.pow(hi, i))
    } else{
      // even powers always become positive so need to worry about sign and lo/hi ordering
      if (lo < 0 && hi < 0) {
        new Interval(scala.math.pow(hi, i), scala.math.pow(lo, i))
      } else if (lo < 0 && hi >= 0) {
        new Interval(0, scala.math.pow(max(-lo, hi), i))
      } else {
        new Interval(scala.math.pow(lo, i), scala.math.pow(hi, i))
      }
    }
  }

  // sin

  // cos

  // e^interval and log OF A POSITIVE interval
  // exp

  // log

  // Signed square root
  // s_sqrt
}
