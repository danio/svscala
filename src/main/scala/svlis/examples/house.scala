package svlis
package examples

import lib._

object House extends App {
  // A pitched roof can be made with 2 slopes and 2 eaves
  val negDiagNormal = new Point(-1, 0, 1)
  val diagNormal = new Point(1, 0, 1)
  val roofApexHeight = 1.8
  val roofCentreLineX = 1.0
  val apexPoint = new Point(roofCentreLineX, 1, roofApexHeight)
  val slope1 = new Set(new Primitive(new Plane(negDiagNormal, apexPoint)))
  val slope2 = new Set(new Primitive(new Plane(diagNormal, apexPoint)))
  val eave1 = new Set(new Primitive(new Plane(Point.Y, new Point(0, 1.8, 0))))
  val eave2 = new Set(new Primitive(new Plane(-Point.Y, new Point(0, 0.2, 0))))
  val roofBase = new Set(new Primitive(new Plane(-Point.Z, new Point(0, 0, 1))))
  // The roof shape can be made of the intersection of the 5 planes
  val roof = slope1 & slope2 & eave1 & eave2 & roofBase

  // build a chimney from a cuboid and some cylinders
  val chimneyBaseWidth = 0.2
  val chimneyBase = Solid.cuboid(
    new Point(roofCentreLineX - chimneyBaseWidth / 2.0, 0.8, roofApexHeight - 0.3),
    new Point(roofCentreLineX + chimneyBaseWidth / 2.0, 1, roofApexHeight + 0.15))
  val chimneyPot1 = Solid.cylinder(new Line(Point.Z, new Point(roofCentreLineX, 0.85, roofApexHeight)), 0.02)
  val chimneyPot2 = Solid.cylinder(new Line(Point.Z, new Point(roofCentreLineX, 0.9, roofApexHeight)), 0.02)
  val chimneyPot3 = Solid.cylinder(new Line(Point.Z, new Point(roofCentreLineX, 0.95, roofApexHeight)), 0.02)
  val chimneyPots = (chimneyPot1 | chimneyPot2 | chimneyPot3) &
    new Set(new Primitive(new Plane(Point.Z, new Point(0, 0, roofApexHeight + 0.22)))) &
    roofBase
  val chimney = chimneyBase | chimneyPots

  // The main part of the house - just a simple cuboid
  val body = Solid.cuboid(new Point(0.3, 0.3, 0.3), new Point(1.7, 1.7, 1.0))

  val house = body | roof | chimney

  // A svLis model is made from a set and a bounding box
  val boundingBox = new Box(new Point(0, 0, 0), new Point(2, 2, 2.5))
  val model = Model(house, boundingBox)

  // Before display a model must be divided into a k-d tree
  // divide is given a user-supplied decision function,
  // here we use a simple function that divides
  // in each dimension in turn until some maximum depth
  // is reached (early terminating at solid/air boxes)
  // Use quite a high depth as only voxel display is currently supported
  val dividedModel = model.divide(decision(18))

  // Currently models can be exported as VRML or STL
  //utils.ModelToVrml.write(dividedModel, "house.wrl", true)
  utils.ModelToStl.write(dividedModel, "house.stl", true)

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
