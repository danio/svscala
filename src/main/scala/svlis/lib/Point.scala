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