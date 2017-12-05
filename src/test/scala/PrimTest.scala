import org.scalatest._
import svlis.lib._
import svlis.utils._


class PrimTest extends FlatSpec with Matchers {
  "primitive" should "support cylinder solids" in {
    val axis = new Line(Point.Z, Point.Origin)
    //val axis = new Line(Point.Z, new Point(0.1, 0.1, 0))

    val radius = 0.5
    val ax = axis.direction
    val cent = axis.origin
    println(cent)
    val srad0 = ax.right
    println(srad0)
    val srad1 = srad0 ^ ax
    println(srad1)
    val hs0 = new Primitive(new Plane(srad0, cent))
    val hs1 = new Primitive(new Plane(srad1, cent))
    val box = new Box(new Point(-1, -1, -1), new Point(1, 1, 1))
    println(box)
    hs0.range(box).member() should be(MemTest.Surface)
    hs1.range(box).member() should be(MemTest.Surface)
//    hs0.range(box) should be(new Interval(-1, 1))
    println(hs0.range(box))
//    hs1.range(box) should be(new Interval(-1, 1))
    println(hs1.range(box))

    val p = new Primitive(radius * radius)
    p.range(box).member() should be(MemTest.Surface)
    val cyl = (hs0 ^ 2) + (hs1 ^ 2) - p
    println((hs0 ^ 2).range(box)) // [1, 1]
    println((hs1 ^ 2).range(box)) // [1, 1]
    println((p).range(box)) // [0, 0]
    println(((hs0 ^ 2) + (hs1 ^ 2)).range(box))
    println(((hs1 ^ 2) - p).range(box))
    println(((hs0 ^ 2) + (hs1 ^ 2) - p).range(box))
    cyl.range(box).member() should be(MemTest.Surface)

    val cyl2 = Primitive.cylinder(axis, radius)
    cyl2.range(box).member() should be(MemTest.Surface)
  }
}
