package svlis.lib

object SetOp extends Enumeration {
  type SetOp = Value
  val None, Union, Intersection = Value
}

class Set(val op: SetOp.Value, val contents: Int, val prim: Primitive, val child_1: Option[Set], val child_2: Option[Set]) {
  // Constructor for set that will be a simple primitive
  def this(prim: Primitive) = this(SetOp.None, prim.contents, prim, None, None)
  // A single plane is a convex polygon - sv_c_flag detects this
  // set_flags(sv_c_flag(p));

  // Constructor for when it's all or nothing.
  // TODO only allow a of Contents.Nothing or Contents.Everything
  def this(a: Int) = this(SetOp.None, a, new Primitive(), None, None)

  // Constructor for set that is compound.
  def this(a: Set, b: Set, optr: SetOp.Value) =
    this(optr, a.contents + b.contents, new Primitive(), Option(a), Option(b))

  // TODO flags support - for convex polygons...
  // see all commented out set_flags, set_flags_priv, flags()

  // Set union
  def |(b: Set): Set= {
    // TODO rationalise
    val c = contents match {
      case Contents.Everything => Set.Everything
      case Contents.Nothing => b
      case 1 => b.contents match {
        case Contents.Everything => Set.Everything
        case Contents.Nothing => this
        case 1 => {
          if (prim.degree > b.prim.degree) {
            new Set(b, this, SetOp.Union)
          } else {
            new Set(this, b, SetOp.Union)
          }
        }
        case _ => new Set(this, b, SetOp.Union)
      }
      case _ => b.contents match {
        case Contents.Everything => Set.Everything
        case Contents.Nothing => this
        case 1 => new Set(b, this, SetOp.Union)
        case _ => {
          if (contents > b.contents) {
            new Set(b, this, SetOp.Union)
          } else {
            new Set(this, b, SetOp.Union)
          }
        }
      }
    }

    // if( (flags() & SV_CV_POL) && (b.flags() & SV_CV_POL) ) c.set_flags_priv(SV_CV_POL);
    c
  }

  // Set intersection
  def &(b: Set): Set = {
    // TODO rationalise
    val c = contents match {
      case Contents.Everything => b
      case Contents.Nothing => Set.Nothing
      case 1 => b.contents match {
        case Contents.Everything => this
        case Contents.Nothing => Set.Nothing
        case 1 => {
          if (prim.degree > b.prim.degree) {
            new Set(b, this, SetOp.Intersection)
          } else {
            new Set(this, b, SetOp.Intersection)
          }
        }
        case _ => new Set(this, b, SetOp.Intersection)
      }
      case _ => b.contents match {
        case Contents.Everything => this
        case Contents.Nothing => Set.Nothing
        case 1 => new Set(b, this, SetOp.Intersection)
        case _ => {
          if (contents > b.contents) {
            new Set(b, this, SetOp.Intersection)
          } else {
            new Set(this, b, SetOp.Intersection)
          }
        }
      }
    }

    // if( (flags() & SV_CV_POL) && (b.flags() & SV_CV_POL) ) c.set_flags_priv(SV_CV_POL);
    c
  }

  // Set complement
  def unary_-(): Set = {
    contents match {
      case Contents.Everything => Set.Everything
      case Contents.Nothing => Set.Nothing
      case 1 => new Set(-prim)
      case _ => { // De Morgan's law
        if (op == SetOp.Union) {
          (-child_1.get & -child_2.get)
        } else {
          (-child_1.get | -child_2.get)
        }
      }

      // TODO attribute complement (set.xx:614)
      // TODO svlis caches the complement, is it beneficial?
    }
  }

  // Range for a box (and winning leaves)
  def range(b: Box) = {
    // TODO logic from set.cxx 1114-1173
    // although I'm not sure that it is needed as I can't see it being used by anything
  }

  // Prune a set to a box
  def prune(b: Box): Set = {
    // TODO rationalise
    val pruned = if (contents <= Contents.Nothing) { // includes Everything
      this
    } else if (contents == 1) {
      val m = prim.range(b).member()
      m match {
        case MemTest.Air => Set.Nothing
        case MemTest.Surface => this
        case MemTest.Solid => Set.Everything
        case _ => this // TODO svlis_error("sv_set::prune(sv_box)", "dud mem test", SV_CORRUPT)
      }
    } else {
      val child1 = child_1.get
      val child2 = child_2.get
      val pruned_1 = child1.prune(b)
      val c_1_same = (pruned_1 == child1)
      if (op == SetOp.Union) {
        pruned_1.contents match {
          case Contents.Everything => pruned_1
          case Contents.Nothing => child2.prune(b)
          case _ => {
            val temp = child2.prune(b)
            if (c_1_same && (child2 == temp)) {
              this
            } else {
              pruned_1 | temp
            }
          }
        }
      }
      else { // SetOp.Intersection
        pruned_1.contents match {
          case Contents.Everything => child2.prune(b)
          case Contents.Nothing => pruned_1
          case _ => {
            val temp = child2.prune(b)
            if (c_1_same && (child2 == temp)) {
              this
            } else {
              pruned_1 & temp
            }
          }
        }
      }
    }

    //if(reg_prune) pruned = pruned.regularize();
    //return(att_prune(pruned, *this, b));
    pruned
  }
}

object Set {
  // equivalent to SV_X, SV_Y, SV_Z, SV_OO, SV_DIAG in enum_def.h
  // The coordinate directions and origin; useful in all sorts
  // of places; also the positive diagonal
  val Nothing = new Set(Contents.Nothing)
  val Everything = new Set(Contents.Everything)
}
