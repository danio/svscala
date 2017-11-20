import org.scalatest.FunSuite
import svlis.lib._

class SetTest extends FunSuite {
  test("Set.fromPlane") {
    val normal = new Point(1, 1, 0)
    val through = new Point(4, 5, 6)
    val set = new Set(new Primitive(new Plane(normal, through)))
    assert(set.contents === 1)
  }

  test("Set.prune") {
    val normal = new Point(1, 1, 0)
    val through = new Point(1, 1, 1)
    val set = new Set(new Primitive(new Plane(normal, through)))

    val pruned = set.prune(SetTest.wholeBox)
    assert(pruned.contents === 1)

    val box2 = new Box(new Point(1.5, 1.5, 1.5), new Point(2, 2, 2))
    val pruned2 = set.prune(box2)
    assert(pruned2.contents === Contents.Nothing)

    val box3 = new Box(new Point(0, 0, 0), new Point(0.5, 0.5, 0.5))
    val pruned3 = set.prune(box3)
    assert(pruned3.contents === Contents.Everything)
  }

  test("Set.union") {
    val normal1 = new Point(1, 0, 0)
    val normal2 = new Point(0, 1, 0)
    val through = new Point(1, 1, 1)
    val plane1 = new Set(new Primitive(new Plane(normal1, through)))
    val plane2 = new Set(new Primitive(new Plane(normal2, through)))
    val set = plane1 | plane2

    assert(set.prune(SetTest.wholeBox).contents === 2)

    assert(set.prune(SetTest.topLeftCorner).contents === Contents.Everything)
    assert(set.prune(SetTest.topRightCorner).contents === Contents.Nothing)

    assert(set.prune(SetTest.bottomLeftCorner).contents === Contents.Everything)
    assert(set.prune(SetTest.bottomRightCorner).contents === Contents.Everything)
  }

  test("Set.intersection") {
    val normal1 = new Point(-1, 1, 0)
    val normal2 = new Point(1, 1, 0)
    val through = new Point(1, 2, 1)
    val slope1 = new Set(new Primitive(new Plane(normal1, through)))
    val slope2 = new Set(new Primitive(new Plane(normal2, through)))
    val set = slope1 & slope2

    assert(set.prune(SetTest.wholeBox).contents === 2)

    assert(set.prune(SetTest.topLeftCorner).contents === Contents.Nothing)
    assert(set.prune(SetTest.topRightCorner).contents === Contents.Nothing)

    assert(set.prune(SetTest.bottomLeftCorner).contents === Contents.Everything)
    assert(set.prune(SetTest.bottomRightCorner).contents === Contents.Everything)
  }
}

object SetTest {
  val wholeBox = new Box(new Point(0, 0, 0), new Point(2, 2, 2))
  // Looking down the Z axis so right is +ve X, top is +ve Y
  val bottomLeftCorner = new Box(new Point(0, 0, 0), new Point(0.3, 0.3, 2))
  val bottomRightCorner = new Box(new Point(1.7, 0, 0), new Point(2, 0.3, 2))
  val topLeftCorner = new Box(new Point(0, 1.7, 0), new Point(0.3, 2, 2))
  val topRightCorner = new Box(new Point(1.7, 1.7, 0), new Point(2, 2, 2))
}