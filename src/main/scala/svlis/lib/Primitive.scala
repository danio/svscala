package svlis.lib

import scala.math._

object PrimKind extends Enumeration {
  type PrimKind = Value
  val Real, Plane, General = Value
}

object PrimOp extends Enumeration {
  type PrimOp = Value
  // TODO remove Zero?
  val Zero, Plus, Minus, Times, Divide, Power, Comp = Value
}

class Primitive(
  val kind: PrimKind.Value,     // Indicates if this is a plane, real or compound
  val flat: Plane,              // Arithmetic is done on planes and reals
  val real: Double,
  private val op: PrimOp.Value, // If compound, this says +, -, *, /, ^, or one of the monadics
  val degree: Long,             // Highest power (trancendentals add one)
  val child_1: Option[Primitive],
  val child_2: Option[Primitive])
{
  // Build a compound primitive from two others and a diadic operator
  def this(a: Primitive, b: Primitive, optr: PrimOp.Value)
  = this(PrimKind.General, new Plane(), 0.0d, optr, Primitive.degree(a, b, optr), Some(a), Some(b))

  // Build a compound primitive from one other and a monadic operator
//  def this(a: Primitive, optr: PrimOp.Value)
//    = this(PrimKind.General, new Plane(), 0.0d, optr, a.degree + 1, Some(a), None)

  def this() = this(PrimKind.Real, Plane.None, 0.0d, PrimOp.Zero, 0, None, None)
  def this(flat: Plane) = this(PrimKind.Plane, flat, 0.0d, PrimOp.Zero, 1, None, None)
  def this(a: Double) = this(PrimKind.Real, Plane.None, a, PrimOp.Zero, 0, None, None)

  // from set.h lines 104-111
  def contents: Int = {
    this match {
      case p: Primitive if p.kind == PrimKind.Real && p.real <= 0.0 => Contents.Everything
      case p: Primitive if p.kind == PrimKind.Real => Contents.Nothing
      case _ => 1
    }
  }

  // The simplifier for the same test

//  sv_primitive dump_scales() const;

  // Characteristic point

//  sv_point point() const;

  // Unique tag

//  sv_integer tag() const;

  // The 5 arithmetic operations

  def +(b: Primitive): Primitive = {
    if (kind == PrimKind.Real && b.kind == PrimKind.Real) {
      new Primitive(real + b.real)
    } else if (kind == PrimKind.Plane && b.kind == PrimKind.Plane) {
      // 2 planes make a plane, but with scaled distance
      val fa = this.flat
      val fb = b.flat
      val n = fa.normal + fb.normal
      val d = fa.d + fb.d
      val scale = n.mod
      new Primitive(scale) * new Primitive(new Plane(n, d))
    } else if (kind == PrimKind.Plane && b.kind == PrimKind.Real) {
      // Plane + real is just a shift
      new Primitive(new Plane(flat.normal, flat.d + b.real))
    } else if (kind == PrimKind.Real && b.kind == PrimKind.Plane) {
      // Plane + real is just a shift
      new Primitive(new Plane(b.flat.normal, flat.d + b.real))
    } else {
      new Primitive(this, b, PrimOp.Plus)
    }
  }

  def -(b: Primitive) = {
    if (kind == PrimKind.Real && b.kind == PrimKind.Real) {
      new Primitive(real - b.real)
    } else if (kind == PrimKind.Plane && b.kind == PrimKind.Plane) {
      // 2 planes make a plane, but with scaled distance
      val fa = this.flat
      val fb = b.flat
      val n = fa.normal - fb.normal
      val d = fa.d - fb.d
      val scale = n.mod
      new Primitive(scale) * new Primitive(new Plane(n, d))
    } else if (kind == PrimKind.Plane && b.kind == PrimKind.Real) {
      // Plane + real is just a shift
      new Primitive(new Plane(flat.normal, flat.d - b.real))
    } else if (kind == PrimKind.Real && b.kind == PrimKind.Plane) {
      // Plane + real is just a shift
      val fb = new Plane(b.flat.normal, flat.d - b.real)
      new Primitive(-fb)
    } else {
      new Primitive(this, b, PrimOp.Minus)
    }
  }

  def *(b: Primitive) = {
    if (kind == PrimKind.Real && b.kind == PrimKind.Real) {
      new Primitive(real * b.real)
    } else {
      new Primitive(this, b, PrimOp.Times)
    }
  }

//  friend sv_primitive operator/(const sv_primitive&, const sv_primitive&);

  def pow(b: Double) = {
    if (kind == PrimKind.Real) {
      new Primitive(scala.math.pow(real, b))
    } else {
      assert (b >= 0)
      // TODO svlis_error("sv_primitive operator^", "negative exponents not supported",SV_WARNING)
      b match {
        case 0.0 => new Primitive(1)
        case 1.0 => this
        case _ => new Primitive(this, new Primitive(b), PrimOp.Power)
      }
    }
  }

  // Only here for compatibility with svlis
  // It's not nice to overload XOR operator with pow!
  def ^(b: Long) = this pow b

  // Test for an undefined set

//  int exists() const { return(prim_info.exists()); }

  // Unique value (effectively the pointer to this set)
  // This is explicitly long not sv_integer

//  long unique() const { return(prim_info.unique()); }

  // Primitives are equal if they point to the same thing.
  // I mean int not sv_integer here:

//  friend int operator==(const sv_primitive& a, const sv_primitive& b) { return(a.unique() == b.unique()); }
//  friend int operator!=(const sv_primitive& a, const sv_primitive& b) { return(!(a == b)); }

  // Value of a primitive for a point

//  sv_real value(const sv_point&) const;

  // Value of a box in a primitive
  def range(b: Box): Interval = {
    kind match {
      case PrimKind.Real => new Interval(0, 0) // TODO svlis_error("sv_primitive::range(box)","primitive is a single constant", SV_WARNING);
      case PrimKind.Plane if op == PrimOp.Zero => flat.range(b)
      case _ => {
        // TODO use more scala techniques, e.g. getOrElse, case classes
        // Logical - T if child is a real
        val c_1 = !child_1.isEmpty && child_1.get.kind == PrimKind.Real
        val c_2 = Primitive.diadic(op) && !child_2.isEmpty && child_2.get.kind == PrimKind.Real
        op match {
          case PrimOp.Plus => {
            if (c_1) {
              if (c_2) {
                // TODO svlis_error("sv_primitive::range(box)","primitive is real+real", SV_WARNING)
                new Interval(child_1.get.real + child_2.get.real, child_1.get.real + child_2.get.real)
              } else {
                child_2.get.range(b) + child_1.get.real
              }
            } else {
              if (c_2) {
                child_1.get.range(b) + child_2.get.real
              } else {
                child_1.get.range(b) + child_2.get.range(b)
              }
            }
          }
          case PrimOp.Minus => {
            if (c_1) {
              if (c_2) {
                // TODO svlis_error("sv_primitive::range(box)","primitive is real-real", SV_WARNING)
                new Interval(child_1.get.real - child_2.get.real, child_1.get.real - child_2.get.real)
              } else {
                -child_2.get.range(b) + child_1.get.real // equivalent to real - interval
              }
            } else {
              if (c_2) {
                child_1.get.range(b) - child_2.get.real
              } else {
                child_1.get.range(b) - child_2.get.range(b)
              }
            }
          }
          case PrimOp.Power => {
            assert (c_2)
            // TODO svlis_error("sv_primitive::range(box)","exponent is a primitive", SV_WARNING)
            if (c_1) {
              // TODO svlis_error("sv_primitive::range(box)","primitive is real^real", SV_WARNING)
              new Interval(scala.math.pow(child_1.get.real, child_2.get.real), scala.math.pow(child_1.get.real, child_2.get.real))
            } else {
              child_1.get.range(b).pow(child_2.get.real)
            }
          }
          case PrimOp.Comp => -child_1.get.range(b)
          // TODO rest of cases from prim.cxx 2108- including fall through of Plane with an op
        }
      }
    }
  }

  // Translate a primitive

//  friend sv_primitive operator+(const sv_primitive&, const sv_point&);
//  friend sv_primitive operator-(const sv_primitive&, const sv_point&);

  // Rotate and mirror

//  sv_primitive spin(const sv_line&, sv_real) const;
//  sv_primitive mirror(const sv_plane&) const;

  // Scale

//  sv_primitive scale(const sv_point&, sv_real) const;

  // 1D Scale along a line

//  sv_primitive scale(const sv_line&, sv_real) const;

  // Complement a sv_primitive
  def unary_-(): Primitive = {
    if (op == PrimOp.Comp) {
      child_1.get
    } else {
      val newFlat = if (kind == PrimKind.Plane) -flat else Plane.None
      val newReal = if (kind == PrimKind.Real) -real else 0.0d
      new Primitive(kind, newFlat, newReal, PrimOp.Comp, degree + 1, Some(this), None)
    }
  }

  // The monadic operators on primitives as functions

//  friend sv_primitive abs(const sv_primitive&);
//  friend sv_primitive sin(const sv_primitive&);
//  friend sv_primitive cos(const sv_primitive&);
//  friend sv_primitive exp(const sv_primitive&);
//  friend sv_primitive s_sqrt(const sv_primitive&);
//  friend sv_primitive sign(const sv_primitive&);

  // The grad at a point

//  sv_point grad(const sv_point& p) const;

  // Special grad at a point for graphics - grad defined at 0s of
  // primitives that are abs()

//  sv_point p_grad(const sv_point& p) const;

  // The range of grads in a box

//  sv_box grad(const sv_box& b) const;
}

object Primitive {
  // TODO more maintainable diadic
  // from enum_def.h:269
  def diadic(op: PrimOp.Value): Boolean = op < PrimOp.Comp
  // from prim.h:137-160
  def degree(a: Primitive, b: Primitive, op: PrimOp.Value): Long = {
    op match {
      case PrimOp.Plus => max(a.degree, b.degree)
      case PrimOp.Minus => max(a.degree, b.degree)
      case PrimOp.Times => a.degree + b.degree
      case PrimOp.Divide => a.degree // Won't work for rationals!
      case PrimOp.Power => a.degree * round(b.real)
      // TODO case _ => svlis_error("hidden_prim constructor", "dud operator",SV_CORRUPT)
    }
  }

  // Make special shapes

  //  This returns an infinitely long cylinder aligned with the line axis
  //  and of radius radius.
  def cylinder(axis: Line, radius: Double): Primitive = {
    val ax = axis.direction
    val cent = axis.origin

    //   Generate a vector perpendicular to the cylinder's axis.
    val srad0 = ax.right

    //   And another perpendicular to both
    val srad1 = srad0 ^ ax

    //  Generate two perpendicular planes intersecting in the axis
    val hs0 = new Primitive(new Plane(srad0, cent))
    val hs1 = new Primitive(new Plane(srad1, cent))

    //  The product of their squares - radius^2 is the cylinder
    val c = (hs0.pow(2)) + (hs1.pow(2)) - new Primitive(radius * radius)
    // TODO needed for ray tracer and slice(): c.set_kind(SV_CYLINDER);
    // TODO support real_d?
//    if (real_d)
//      s_sqrt(c)
//    else
        c
  }

  //  friend sv_primitive p_cone(const sv_line&, sv_real);

  def sphere(centre: Point, radius: Double): Primitive = {
    val xhs = new Primitive(new Plane(Point.X, centre))
    val yhs = new Primitive(new Plane(Point.Y, centre))
    val zhs = new Primitive(new Plane(Point.Z, centre))

    val s = xhs.pow(2) + yhs.pow(2) + zhs.pow(2) - new Primitive(radius * radius)
//    TODO needed for ray tracer and slice(): s.set_kind(SV_SPHERE);
    // TODO support real_d?
//    if (real_d)
//      return(s_sqrt(s));
//    else
//      return(s);
    s
  }

  //  friend sv_primitive p_torus(const sv_line&, sv_real, sv_real);
  //  friend sv_primitive p_cyclide(const sv_line&, const sv_point&,
  //    sv_real, sv_real, sv_real);
}