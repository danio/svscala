package svlis.lib

// a 3D box comprising three intervals
// from interval.h:402-696 and interval.cxx:313-597
class Box(val xi: Interval, val yi: Interval, val zi: Interval) {
  def this(lo: Point, hi: Point) = this(new Interval(lo.x, hi.x), new Interval(lo.y, hi.y), new Interval(lo.z, hi.z))

  override def toString(): String = s"Box <${xi}, ${yi}, ${zi}>"

  // vol: Double

  // diag_sq: Double

  def centroid: Point =
    new Point((xi.lo + xi.hi) / 2, (yi.lo + yi.hi) / 2, (zi.lo + zi.hi) / 2)

  // member(p: Point): MemTest.Value

  // inside(b: Box): Int

  // The lexical order of a box's corners
  def corner(i: Int): Point = {
    if (i < 0 || i > 7) {
      // TODO svlis_error("box_corner","silly corner number",SV_WARNING);
      Point.Origin
    } else {
      val x = if ((i & 4) == 0) xi.lo else xi.hi
      val y = if ((i & 2) == 0) yi.lo else yi.hi
      val z = if ((i & 1) == 0) zi.lo else zi.hi
      new Point(x, y, z)
    }
  }

  // norm(a: Box): Box

  // tag: Int

  // boxEdge: Unit

  // tet*

  // lineBox(Line, Box): Interval

  // lIntPlane(Line, Plane, Interval): Interval

  // boxSpread(Box, Point, Point, Double, Double): Bsp.Value

  // operators

  // mathematical primitives

  // &(b: Box): Box

  // |(b: Box): Box
}
