import org.scalatest._
import collection.mutable.HashMap
import svlis.lib._
import svlis.utils._

class ModelSpec extends FlatSpec with Matchers {

  "A Model" should "be constructable from a set" in {
    val normal = new Point(1, 1, 0)
    val through = new Point(1, 1, 1)
    val set = new Set(new Primitive(new Plane(normal, through)))

    val box = new Box(new Point(0, 0, 0), new Point(2, 2, 2))
    val model = Model(set, box)
  }

  it should "divide into solid and air" in {
    val normal = new Point(-1, 1, 0)
    val through = new Point(1, 1, 1)
    val set = new Set(new Primitive(new Plane(normal, through)))

    val box = new Box(new Point(0, 0, 0), new Point(2, 2, 2))
    val model = Model(set, box)

    val divided = model.divide(ModelSpec.decision(4))

    val boxContents = new HashMap[Int, Int]()
    def checkBox(m: Model, level: Int) = {
      // the plane is through the line x = y so should divided as air/solid on y > x
      if (m.box.yi.lo > m.box.xi.hi) m.set.contents should be(Contents.Nothing)
      if (m.box.yi.hi < m.box.xi.lo) m.set.contents should be(Contents.Everything)
      boxContents(m.set.contents) = boxContents.getOrElseUpdate(m.set.contents, 0) + 1
    }
    divided.walk(checkBox)
    assert(boxContents(Contents.Nothing) > 0)
    assert(boxContents(Contents.Everything) > 0)
    assert(boxContents(1) > 0)
  }

  it should "support intersected sets" in {
    val normal1 = new Point(-1, 1, 0)
    val normal2 = new Point(1, 1, 0)
    val through = new Point(1, 2, 1)
    val slope1 = new Set(new Primitive(new Plane(normal1, through)))
    val slope2 = new Set(new Primitive(new Plane(normal2, through)))
    val set = slope1 & slope2

    val box = new Box(new Point(0, 0, 0), new Point(2, 2, 2))
    val model = Model(set, box)

    val divided = model.divide(ModelSpec.decision(8))

    val boxContents = new HashMap[Int, Int]()
    def checkBox(m: Model, level: Int) = {
      // the plane is through the line x = y so should divided as air/solid on y > x
      if (m.box.xi.hi < 0.26 && m.box.yi.lo > 1.6) m.set.contents should be(Contents.Nothing)
      if (m.box.xi.hi > 1.74 && m.box.yi.lo > 1.6) m.set.contents should be(Contents.Nothing)
      if (m.box.xi.hi < 0.26 && m.box.yi.hi < 0.26) m.set.contents should be(Contents.Everything)
      if (m.box.xi.hi > 1.74 && m.box.yi.hi < 0.26) m.set.contents should be(Contents.Everything)
      boxContents(m.set.contents) = boxContents.getOrElseUpdate(m.set.contents, 0) + 1
    }
    divided.walk(checkBox)
    assert(boxContents(Contents.Nothing) > 0)
    assert(boxContents(Contents.Everything) > 0)
    assert(boxContents(1) > 0)

    //ModelDump.dump(divided)
    ModelToVrml.write(divided, "divided2.wrl", true)
  }
}

object ModelSpec {
  def mid(iv: Interval): Double = iv.lo + (iv.hi - iv.lo) / 2.0d

  def decision(maxDepth: Int)(m: Model, level: Int): DivideDecision = {
    if (level > maxDepth || m.set.contents <= Contents.Nothing) new DivideDecision(ModelKind.Leaf, 0)
    else level % 3 match {
      case 0 => new DivideDecision(ModelKind.XDiv, mid(m.box.xi))
      case 1 => new DivideDecision(ModelKind.YDiv, mid(m.box.yi))
      case 2 => new DivideDecision(ModelKind.ZDiv, mid(m.box.zi))
    }
  }

  def decision(m: Model, level: Int): DivideDecision = decision(7)(m, level)
}

