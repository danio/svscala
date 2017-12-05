package svlis.lib

// From geometry.h/.cxx

class Line(val direction: Point, val origin: Point) {
  // direction
  // origin: Point

  def this() = this(Point.Z, Point.Origin)
}

object Line {
  def fromEnds(a: Point, b: Point) = new Line(a.norm, b)
}
