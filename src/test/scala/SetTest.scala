import org.scalatest.FunSuite
import svlis._

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

    val box = new Box(new Point(0, 0, 0), new Point(2, 2, 2))
    val pruned = set.prune(box)
    assert(pruned.contents === 1)

    val box2 = new Box(new Point(1.5, 1.5, 1.5), new Point(2, 2, 2))
    val pruned2 = set.prune(box2)
    assert(pruned2.contents === Contents.Nothing)

    val box3 = new Box(new Point(0, 0, 0), new Point(0.5, 0.5, 0.5))
    val pruned3 = set.prune(box3)
    assert(pruned3.contents === Contents.Everything)
  }
}