package svlis.lib

import scala.math._

// From geometry.h/.cxx

class Point(val x: Double, val y: Double, val z: Double)
{
  override def toString(): String = f"Point [$x%.3f, $y%.3f, $z%.3f]"

  // Modulus and normalization

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

  // Transformations
  // spin, mirror, scale

  // Monadic minus, diadic + and -

  def unary_-(): Point = {
    new Point(-x, -y, -z)
  }

  def +(b: Point) = new Point(x + b.x, y + b.y, z + b.z)

  def -(b: Point) = new Point(x - b.x, y - b.y, z - b.z)

  // from geometry.h:442
  // def +(b: Plane) = b + a

  // Multiply/divide by a real

  def *(b: Double): Point = new Point(x * b, y * b, z * b)

  def /(b: Double): Point = {
    assert(b != 0.0) // TODO better handle error
    //  svlis_error("sv_point::operator/","division by 0",SV_WARNING);
    new Point(x / b, y / b, z / b)
  }

  // Scalar and vector product

  def *(b: Point): Double = {
    x * b.x + y * b.y + z * b.z
  }

  def ^(b: Point): Point = {
    new Point(
      this.y * b.z - b.y * this.z,
      b.x * this.z - this.x * b.z,
      this.x * b.y - b.x * this.y
    )
  }

  // finds the squared distance between two points
  // dist_2

  // Generate a unit-length point at right angles to another
  def right: Point = {
    if (abs(x) < 0.1) {
      (this ^ Point.X).norm
    } else {
      (this ^ Point.Y).norm
    }
  }

  // Set up a set of orthogonal axes with p as one (w)
  // sv_axes

  // Two points the same?
  // same
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