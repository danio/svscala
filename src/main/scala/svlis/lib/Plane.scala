package svlis.lib

import svlis.lib._

// From geometry.h 295- and geometry.cxx 299-

// A flat plane in space.
// The plane is:
//    normal.x * X + normal.y * Y + normal.z * Z + d = 0

class Plane(val normal: Point, val d: Double) {
  // Null constructor
  def this() = this(new Point(0, 0, 1), 0)

  // Constructor takes a normal vector (a) and a point through which
  // the plane is to pass (b).
  def this(a: Point, b: Point) = this(a.norm, -b * a.norm)

  // Signed distance of a sv_point from a plane, and value of a box
  // value

  // from interval.h 700-704
  def range(b: Box): Interval = {
    b.xi * normal.x + b.yi * normal.y + b.zi * normal.z + d
  }

  // Transformations
  // spin, mirror, scale

  // Intersections
  // point(l: Line) = l.point(plane_line_t(this, l))
  // planes_line, plane_line_t, planes_point

  // Monadic - means complement
  def unary_-(): Plane = {
    new Plane(-normal, -d)
  }

  // As usual, adding a point effects a translation
  // +(b: Point) = new Plane(normal, b - d * normal)
  // -(b: Point) = this + (-b)

  // Two planes the same?
  // same(b: Plane)
}

object Plane {
  def None = new Plane()
}
