package svlis.lib

object Solid {
  def Cuboid(l: Point, h: Point) = {
    val low =
      new Set(new Primitive(new Plane(-Point.X, l))) &
      new Set(new Primitive(new Plane(-Point.Y, l))) &
      new Set(new Primitive(new Plane(-Point.Z, l)))
    val high =
      new Set(new Primitive(new Plane(Point.X, h))) &
      new Set(new Primitive(new Plane(Point.Y, h))) &
      new Set(new Primitive(new Plane(Point.Z, h)))
    low & high;
  }
}
