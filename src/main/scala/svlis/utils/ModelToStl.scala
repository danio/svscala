package svlis
package utils

import java.io._
import svlis.lib._

// Write to STL file
// See https://en.wikipedia.org/wiki/STL_(file_format)

class ModelToStl(val pw: PrintWriter, val writeAxes: Boolean = false, val writeBoxes: Boolean = false, val writeVoxels: Boolean = true, val writeFaces: Boolean = true) {
  def writePoint(p: Point) {
    pw.write(s"\t\tvertex ${p.x} ${p.y} ${p.z}\n")
  }

  def writeTriangle(p1: Point, p2: Point, p3: Point) {
    val normal = (p2 - p1) ^ (p3 - p1)
    pw.write(s"facet normal ${normal.x} ${normal.y} ${normal.z}\n")
    pw.write("\touter loop\n")
    writePoint(p1)
    writePoint(p2)
    writePoint(p3)
    pw.write("\tendloop\n")
    pw.write("endfacet\n")
  }

  def writeFace(p1: Point, p2: Point, p3: Point, p4: Point): Unit = {
    writeTriangle(p1, p4, p2)
    writeTriangle(p2, p4, p3)
  }

  def writeCuboid(b: Box) {
    // Assumes cuboid has lexical ordering of corners in X, Y, Z
    // Face ordering using right handed rule
    // Front
    writeFace(b.corner(0), b.corner(1), b.corner(5), b.corner(4))
    // Left
    writeFace(b.corner(2), b.corner(3), b.corner(1), b.corner(0))
    // Back
    writeFace(b.corner(6), b.corner(7), b.corner(3), b.corner(2))
    // Right
    writeFace(b.corner(4), b.corner(5), b.corner(7), b.corner(6))
    // Top
    writeFace(b.corner(1), b.corner(3), b.corner(7), b.corner(5))
    // Bottom
    writeFace(b.corner(2), b.corner(0), b.corner(4), b.corner(6))
  }

  def writeModelBox(m: Model) {
    if (m.set.contents > 0) {
      writeCuboid(m.box)
    }
  }

  def writeModel(m: Model, level: Int) {
    m match {
      case LeafModel(_, _, _) => writeModelBox(m)
      case DividedModel(_, _, _, c1, c2) => ()
    }
  }

  def writeArrow(pre: String, direction: Point, colour: Point, baseSize: Double, scale: Double, pointRotation: Point, rotation: Double) {
    val lineEnd = direction * baseSize
//    writeShape(pre + " ",
//      (pre) => writeLine(pre)(Point.Origin, lineEnd),
//      () => pw.write(s"emissiveColor ${colour.x} ${colour.y} ${colour.z}"))
  }

  def writeAxes(pre: String, m: Model) {
    writeArrow(pre, Point.X, Color.Red, m.box.xi.hi, 0.4, Point.Z, -1.57)
    writeArrow(pre, Point.Y, Color.Green, m.box.yi.hi, 0.4, Point.Origin, 0)
    writeArrow(pre, Point.Z, Color.Blue, m.box.yi.hi, 0.4, Point.X, 1.57)
  }

  def write(m: Model) {
    pw.write("solid name\n")
    m.walk(writeModel)
    pw.write("endsolid name\n")
  }
}

object ModelToStl {
  def apply(m: Model, filename: String, writeAxes: Boolean = false, writeBoxes: Boolean = false, writeVoxels: Boolean = true, writeFaces: Boolean = true): ModelToStl = {
    val file = new File(filename)
    val pw = new PrintWriter(file, "UTF-8")
    new ModelToStl(pw, writeAxes, writeBoxes, writeVoxels, writeFaces)
  }

  def getSvlisVersion(): String = "TODO"

  def write(m: Model, filename: String, writeAxes: Boolean = false, writeBoxes: Boolean = false, writeVoxels: Boolean = true, writeFaces: Boolean = true) {
    val m2s = apply(m, filename, writeAxes, writeBoxes, writeVoxels, writeFaces)
    m2s.write(m)
    m2s.pw.close()
  }
}
