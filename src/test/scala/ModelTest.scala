import org.scalatest._
import svlis._
import svlis.utils._

class ModelSpec extends FlatSpec with Matchers {

  "A Model" should "be constructable from a set" in {
    val normal = new Point(1, 1, 0)
    val through = new Point(1, 1, 1)
    val set = new Set(new Primitive(new Plane(normal, through)))

    val box = new Box(new Point(0, 0, 0), new Point(2, 2, 2))
    val model = Model(set, box)

    def mid(iv: Interval): Double = iv.lo + (iv.hi - iv.lo) / 2.0d

    def decision(m: Model, level: Int): DivideDecision = {
      if (level > 3) new DivideDecision(ModelKind.Leaf, 0)
      else level % 3 match {
        case 0 => new DivideDecision(ModelKind.XDiv, mid(m.box.xi))
        case 1 => new DivideDecision(ModelKind.YDiv, mid(m.box.yi))
        case 2 => new DivideDecision(ModelKind.ZDiv, mid(m.box.zi))
      }
    }

    val divided = model.divide(decision)
    ModelDump.dump(divided)
  }
}
