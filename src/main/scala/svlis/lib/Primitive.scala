package svlis.lib

object PrimKind extends Enumeration {
  type PrimKind = Value
  val Real, Plane = Value
}

object PrimOp extends Enumeration {
  type PrimOp = Value
  // TODO remove Zero?
  val Zero, Plus, Minus, Times, Divide, Power, Comp = Value
}

class Primitive(
  val kind: PrimKind.Value, // Indicates if this is a plane, real or compound
  val flat: Plane,          // Arithmetic is done on planes and reals
  val real: Double,
  private val op: PrimOp.Value,         // If compound, this says +, -, *, /, ^, or one of the monadics
  val degree: Int,          // Highest power (trancendentals add one)
  val child_1: Option[Primitive],
  val child_2: Option[Primitive])
{
  // Build a compound primitive from one other and a monadic operator
//  def this(a: Primitive, op: PrimOp.Value)
//    = this(PrimKind.General, new Plane(), 0.0d, op, a.degree + 1, Some(a), None)

  def this() = this(PrimKind.Real, Plane.None, 0.0d, PrimOp.Zero, 0, None, None)
  def this(flat: Plane) = this(PrimKind.Plane, flat, 0.0d, PrimOp.Zero, 1, None, None)

  // from set.h lines 104-111
  def contents: Int = {
    this match {
      case p: Primitive if p.kind == PrimKind.Real && p.real <= 0.0 => Contents.Everything
      case p: Primitive if p.kind == PrimKind.Real => Contents.Nothing
      case _ => 1
    }
  }

  // Value of a box in a primitive
  def range(b: Box): Interval = {
    kind match {
      case PrimKind.Real => new Interval(0, 0) // TODO svlis_error("sv_primitive::range(box)","primitive is a single constant", SV_WARNING);
      case PrimKind.Plane if op == PrimOp.Zero => flat.range(b)
      case _ => op match {
        case PrimOp.Comp => -child_1.get.range(b)
      }
      // TODO rest of cases from prim.cxx 2108- including fall through of Plane with an op
    }
  }

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
}