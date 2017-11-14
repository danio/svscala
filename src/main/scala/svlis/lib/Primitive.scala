package svlis.lib

object PrimKind extends Enumeration {
  type PrimKind = Value
  val Real, Plane, General = Value
}

object PrimOp extends Enumeration {
  type PrimOp = Value
  val Zero, Plus, Minus, Times, Divide, Power = Value
}

class Primitive(val kind: PrimKind.Value, val flat: Plane, val real: Double, op: PrimOp.Value, degree: Int) {
  def this() = this(PrimKind.Real, new Plane(), 0.0d, PrimOp.Zero, 1)
  def this(flat: Plane) = this(PrimKind.Plane, flat, 0.0d, PrimOp.Zero, 1)

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
        // TODO cases from prim.cxx 2108- including fall through of Plane with an op
    }
  }
}