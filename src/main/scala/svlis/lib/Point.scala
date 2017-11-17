package svlis.lib

import scala.math.sqrt

// From geometry.h/.cxx

class Point(val x: Double, val y: Double, val z: Double)
{
  def unary_-(): Point = {
    new Point(-x, -y, -z)
  }

  // Multiply/divide by a real

  def *(b: Double): Point = new Point(x * b, y * b, z * b)

  def /(b: Double): Point = {
    assert(b != 0.0) // TODO better handle error
    //  svlis_error("sv_point::operator/","division by 0",SV_WARNING);
    new Point(x / b, y / b, z / b)
  }

  // Scalar product
  def *(b: Point): Double = {
    x * b.x + y * b.y + z * b.z
  }

  def mod: Double = {
    sqrt(this * this)
  }

  def norm: Point = {
    val b: Double = mod
    assert(b != 0.0) // TODO better handle error
    // svlis_error("sv_point::norm()",
    // "attempt to normalize zero-size point", SV_WARNING);
    this / b
  }
}

object Point {
  // equivalent to SV_X, SV_Y, SV_Z, SV_OO, SV_DIAG in enum_def.h
  // The coordinate directions and origin; useful in all sorts
  // of places; also the positive diagonal
  val X = new Point(1, 0, 0)
  val Y = new Point(0, 1, 0)
  val Z = new Point(0, 0, 1)
  val Origin = new Point(0, 0, 0)
  val Diag = new Point(1, 1, 1)
}