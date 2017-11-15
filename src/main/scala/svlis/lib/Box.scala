package svlis.lib

// a 3D box comprising three intervals
class Box(val xi: Interval, val yi: Interval, val zi: Interval) {
  def this(lo: Point, hi: Point) = this(new Interval(lo.x, hi.x), new Interval(lo.y, hi.y), new Interval(lo.z, hi.z))

  override def toString(): String = s"Box <${xi}, ${yi}, ${zi}>"

  def centroid(): Point =
    new Point((xi.lo + xi.hi) / 2, (yi.lo + yi.hi) / 2, (zi.lo + zi.hi) / 2)
}
