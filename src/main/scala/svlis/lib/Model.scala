package svlis.lib

// A Model is a Set within a Box in space
// In the original svLis this was a list of sets
// rather than a set but for simplicity I have just implenented
// it as a set

// Models can be divided into sub-models (k-d tree of the boxes)
// at each division the model Set is pruned to the child boxes
// giving child sets for the child models

// The divider is supplied with a decision function that decides
// in which direction to divide the box and when to stop dividing

object ModelKind extends Enumeration {
  type ModelKind = Value
  val Leaf, XDiv, YDiv, ZDiv = Value
}


// The resut of a divide decision function
class DivideDecision(val k: ModelKind.Value, val cut: Double)
//  k is the box cut direction (or leaf if no cut decided)
//  cut is the position of the box cut


//class Model(_s: Set, val b: Box) {
trait Model {
  // store root
  //val set = _s.prune(b)
  //val parent = Null
  def root: Model
  def child_1: Option[Model]
  def child_2: Option[Model]
  def set: Set
  def box: Box

  // decision function is of form:
  // (model: Model, level: Int) => (k: ModelKind.Value cut: Double))
  // where:
  //  model is the model at this depth
  //  level is the recursion depth

  // model.h:423
  // sv_model sv_model::divide(void* vp, sv_decision svd)
  def divide(decision: (Model, Int) => DivideDecision): Model = {
    redivide(set, decision)
  }

  // model.cxx:241
  // void redivide_r(void* vsdd)
  // TODO should be private
  def redivide_r(s: Set, decision: (Model, Int) => DivideDecision, level: Int): Model = {
    val decisionResult = decision(this, level)
    if (decisionResult.k == ModelKind.Leaf) {
      this
    } else {
      // TODO check if this is customisable in svlis
      val swell_fac = 0.01 // To stop things falling in the gaps

      val cut = decisionResult.cut
      val (box1, box2) = Model.createDividedBoxes(box, decisionResult)
      val c_1 = new LeafModel(s, box1, root).redivide_r(s, decision, level + 1)
      val c_2 = new LeafModel(s, box2, root).redivide_r(s, decision, level + 1)
      new DividedModel(s, box, root, c_1, c_2)
    }
  }

  // TODO move (and recursive part) out into function for just dividing?
  // model.cxx:442
  // sv_model sv_model::redivide(const sv_set_list& s, void* vp, sv_decision decision )
  def redivide(s: Set, decision: (Model, Int) => DivideDecision): Model = {
    redivide_r(s, decision, 0)
  }

  // Depth-first walk of the model's children tree
  def walk(f: (Model, Int) => Unit, level: Int = 0): Unit = {
    f(this, level)
    this match {
      case LeafModel(_, _, _) => ()
      case DividedModel(_, _, _, c1, c2) => {
        c1.walk(f, level + 1)
        c2.walk(f, level + 1)
      }
    }
  }
}

case class UndividedModel(s: Set, b: Box) extends Model {
  override def root = this
  override def child_1 = None
  override def child_2 = None
  override def set = s.prune(b)
  override def box = b
}

case class LeafModel(s: Set, b: Box, r: Model) extends Model {
  override def root = r
  override def child_1 = None
  override def child_2 = None
  override def set = s.prune(b)
  override def box = b
}

case class DividedModel(s: Set, b: Box, r: Model, c1: Model, c2: Model) extends Model {
  override def root = r
  override def child_1 = Some(c1)
  override def child_2 = Some(c2)
  override def set = s.prune(b)
  override def box = b
}

object Model {
  def apply(s: Set, b: Box): Model = new UndividedModel(s, b)

  private def createDividedBoxes(box: Box, d: DivideDecision): (Box, Box) = {
    val cut = d.cut
    d.k match {
      case ModelKind.XDiv => {
        val (iv1, iv2) = createDividedIntervals(box.xi, cut)
        (new Box(iv1, box.yi, box.zi), new Box(iv2, box.yi, box.zi))
      }
      case ModelKind.YDiv => {
        val (iv1, iv2) = createDividedIntervals(box.yi, cut)
        (new Box(box.xi, iv1, box.zi), new Box(box.xi, iv2, box.zi))
      }
      case ModelKind.ZDiv => {
        val (iv1, iv2) = createDividedIntervals(box.zi, cut)
        (new Box(box.xi, box.yi, iv1), new Box(box.xi, box.yi, iv2))
      }
    }
  }

  private def createDividedIntervals(iv: Interval, cut: Double): (Interval, Interval) = {
    // TODO check if this is customisable in svlis
    val swell_fac = 0.01 // To stop things falling in the gaps

    val iv1 = new Interval(iv.lo, cut + (cut - iv.lo) * swell_fac)
    val iv2 = new Interval(cut - (iv.hi - cut) * swell_fac, iv.hi)
    (iv1, iv2)
  }
}
