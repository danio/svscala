package svlis.lib

object Solid {
  def cuboid(l: Point, h: Point): Set = {
    val low =
      new Set(new Primitive(new Plane(-Point.X, l))) &
        new Set(new Primitive(new Plane(-Point.Y, l))) &
        new Set(new Primitive(new Plane(-Point.Z, l)))
    val high =
      new Set(new Primitive(new Plane(Point.X, h))) &
        new Set(new Primitive(new Plane(Point.Y, h))) &
        new Set(new Primitive(new Plane(Point.Z, h)))
    low & high
  }

  def cuboid(b: Box): Set = {
    val l = b.corner(0)
    val h = b.corner(7)
    cuboid(l, h)
  }

  // thinCuboid(l: Point, h: Point): Set

  // thinCuboid(b: Box): Set

  // def tetrahedron(v: Array[Point]): Set = {
//    sv_set result = sv_set(SV_EVERYTHING);
//    sv_point p[3], q[3];
//    int count;
//    sv_real vol;
//    sv_plane flat;
//    for (int i = 0; i < 4; i++)
//    {
//      count = 0;
//      for(int j = 0; j < 4; j++)
//      {
//        if(j != i)
//        {
//          p[count] = v[j] - v[i];
//          q[count] = v[j];
//          count++;
//        }
//      }
//      vol = p[0].x*(p[1].y*p[2].z - p[2].y*p[1].z) -
//      p[1].x*(p[0].y*p[2].z - p[2].y*p[0].z) +
//      p[2].x*(p[0].y*p[1].z - p[1].y*p[0].z);
//      if(vol < 0)
//        flat = sv_plane(q[0], q[2], q[1]);
//      else
//      flat = sv_plane(q[0], q[1], q[2]);
//      result = result & sv_set(sv_primitive(flat));
//    }
//    return(result);
//  }

  //  This returns an infinitely long cylinder aligned with the line axis
  //  and of radius radius.
  def cylinder(axis: Line, radius: Double): Set = new Set(Primitive.cylinder(axis, radius))

  // def thinCylinder(axis: Line, radius: Double): Set = Set(abs(Primitive.cylinder(axis, radius)))

  // cone

  // sphere

  //  This returns a torus with its major circle in a plane through the
  //  origin of the line axis and perpendicular to axis.  tor_rad is the
  //  major radius, tor_sect_rad the minor.
  // torus

  //  This returns a cyclide with its major circle in a plane through the
  //  origin of the line axis and perpendicular to axis.
  //  sym is a vector that defines the plane of symmetry - it _must_ not
  //  be parallel to the axis.
  //  a is the major cyclide radius, m the minor +/- c.
  // cyclide
}
