package svlis
package examples

import lib._

object Main extends App {
  // A pitched roof can be made with 2 slopes and 2 eaves
  val xyNegDiagNormal = new Point(-1, 1, 0)
  val xyDiagNormal = new Point(1, 1, 0)
  val apexPoint = new Point(1, 2, 1)
  val slope1 = new Set(new Primitive(new Plane(xyNegDiagNormal, apexPoint)))
  val slope2 = new Set(new Primitive(new Plane(xyDiagNormal, apexPoint)))
  val zUnitVector = new Point(0, 0, 1)
  val eave1 = new Set(new Primitive(new Plane(zUnitVector, new Point(0, 0, 1.8))))
  val eave2 = new Set(new Primitive(new Plane(-zUnitVector, new Point(0, 0, -1.8))))
  val yUnitVector = new Point(0, 1, 0)
  val roofBase = new Set(new Primitive(new Plane(-yUnitVector, new Point(0, 1.5, 0))))
  // The roof shape can be made of the intersection of the 5 planes
  val roof = slope1 & slope2 & eave1 & eave2 & roofBase

  // A svLis model is made from a set and a bounding box
  val boundingBox = new Box(new Point(-2, -2, -2), new Point(2, 2, 2))
  val model = Model(roof, boundingBox)

  // Before display a model must be divided into a k-d tree
  // divide is given a user-supplied decision function,
  // here we use a simple function that divides
  // in each dimension in turn until some maximum depth
  // is reached (early terminating at solid/air boxes)
  // Use quite a high depth as only voxel display is currently supported
  val dividedModel = model.divide(decision(12))

  // Currently models can be exported as VRML
  utils.ModelToVrml.write(dividedModel, "house.wrl", true)


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