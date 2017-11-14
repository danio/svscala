package svlis.lib

class Interval(val lo: Double, val hi: Double) {
  override def toString(): String = s"Interval [${lo}, ${hi}]"

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

  // Intervals and reals

  def +(b: Double): Interval = {
    new Interval(lo + b, hi + b)
  }

  def *(b: Double): Interval = {
    if (b > 0.0)
      new Interval(lo * b, hi * b)
    else
      new Interval(hi * b, lo * b)
  }

  // Interval arithmetic

  def +(b: Interval): Interval = {
    new Interval(lo + b.lo, hi + b.hi)
  }

}
