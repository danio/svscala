package svlis
package utils

import java.io._
import svlis.lib._

// VRML export logic from polygon.cxx lines 1861-2116

class ModelToVrml(val pw: PrintWriter, val writeAxes: Boolean = false, val writeBoxes: Boolean = false, val writeVoxels: Boolean = true, val writeFaces: Boolean = true) {
  def writeLine(pre: String)(from: Point, to: Point) = {
    pw.write(s"${pre}geometry IndexedLineSet {\n")
    pw.write(s"${pre} coordIndex [ 0, 1, -1 ]\n")
    pw.write(s"${pre} coord Coordinate { point [ ${from.x} ${from.y} ${from.z} , ${to.x} ${to.y} ${to.z} ] }\n")
    pw.write(s"${pre}}\n")
  }

  def writeBox(pre: String)(sx: Double, sy: Double, sz: Double) = {
    pw.write(s"${pre}geometry Box { size $sx $sy $sz }\n")
  }

  def writeCone(pre: String)(bottomRadius: Double, height: Double) = {
    pw.write("geometry Cone {\n")
    pw.write(s" bottomRadius $bottomRadius\n")
    pw.write(s" height $height\n")
    pw.write(" side TRUE\n")
    pw.write(" bottom TRUE\n")
    pw.write("}\n")
  }

  def writeTransformedChildren(pre: String, writeTransform: () => Unit, writeChildren: () => Unit) = {
    pw.write(s"${pre}Transform {\n")
    pw.write(s"${pre} ")
    writeTransform()
    pw.write(s"\n")
    pw.write(s"${pre} children [\n")
    writeChildren()
    pw.write(s"${pre} ]\n")
    pw.write(s"${pre}}\n")
  }

  def writeTranslatedChildren(pre: String, tx: Double, ty: Double, tz: Double, writeChildren: () => Unit) = {
    writeTransformedChildren(pre, () => pw.write(s"translation $tx $ty $tz"), writeChildren)
  }

  def writeShape(pre: String, writeAShape: (String) => Unit, writeMaterial: () => Unit = () => ()) = {
    pw.write(s"${pre}Shape {\n")
    writeAShape(pre + " ")
    pw.write(s"${pre} appearance Appearance { \n")
    pw.write(s"${pre}  material Material { ")
    writeMaterial()
    pw.write("}\n")
    pw.write(s"${pre} }\n")
    pw.write(s"${pre}}\n")
  }

  def writeTransformedShape(pre: String, tx: Double, ty: Double, tz: Double, writeAShape: (String) => Unit, writeMaterial: () => Unit = () => ()) = {
    writeTranslatedChildren(pre, tx, ty, tz, () => {
      writeShape(pre + " ", (pre) => writeAShape(pre + " "), () => writeMaterial())
    })
  }

  def writeCuboid(b: Box, color: Point, transparency: Double = 0.0) {
    val sx = b.xi.hi - b.xi.lo
    val sy = b.yi.hi - b.yi.lo
    val sz = b.zi.hi - b.zi.lo
    // VRML boxes are centred about the origin, so they
    // need to be translated by half their size and also their box minimum
    val tx = sx / 2 + b.xi.lo
    val ty = sy / 2 + b.yi.lo
    val tz = sz / 2 + b.zi.lo
    val pre = "    "
    writeTransformedShape(pre, tx, ty, tz,
      (pre) => writeBox(pre)(sx, sy, sz),
      () => {
        if (transparency > 0.0) {
          pw.write(s"transparency $transparency ")
        }
        // TODO pw.write(" diffuseColor ${color.x} ${color.y} ${color.z}")
      })
  }

  def writeModelBox(m: Model) = {
    if (m.set.contents != Contents.Nothing) {
      writeCuboid(m.box, new Point(1, 0, 0), if (m.set.contents != Contents.Everything) 0.2 else 0.0)
    }
  }

  def writeModel(m: Model, level: Int) = {
    m match {
      case LeafModel(_, _, _) => writeModelBox(m)
      case DividedModel(_, _, _, c1, c2) => ()
    }
  }

  def writeArrow(pre: String, direction: Point, colour: Point, baseSize: Double, scale: Double, pointRotation: Point, rotation: Double) = {
    val lineEnd = direction * baseSize
    writeShape(pre + " ",
      (pre) => writeLine(pre)(Point.Origin, lineEnd),
      () => pw.write(s"emissiveColor ${colour.x} ${colour.y} ${colour.z}"))
    val coneHeight = baseSize * scale / 2.0
    val translation = lineEnd * (1.0 + coneHeight)
    writeTransformedChildren(pre,
      () => {
        pw.write(s" translation ${translation.x} ${translation.y} ${translation.z}\n")
        pw.write(s" rotation ${pointRotation.x} ${pointRotation.y} ${pointRotation.z} $rotation\n")
      },
      () => {
        writeShape(pre + " ",
          (pre) => writeCone(pre)(coneHeight / 2.0, coneHeight),
          () => pw.write(s"diffuseColor ${colour.x} ${colour.y} ${colour.z}"))
      })
  }

  def writeAxes(pre: String, m: Model) = {
    writeArrow(pre, Point.X, Color.Red, m.box.xi.hi, 0.4, Point.Z, -1.57)
    writeArrow(pre, Point.Y, Color.Green, m.box.yi.hi, 0.4, Point.Origin, 0)
    writeArrow(pre, Point.Z, Color.Blue, m.box.yi.hi, 0.4, Point.X, 1.57)
  }

  def writeHeader() = {
    pw.write("#VRML V2.0 utf8\n\n")
    pw.write("WorldInfo {\n")
    pw.write(" info [\n")
    pw.write("  \"Created by svLis version " + ModelToVrml.getSvlisVersion() + " - ")
    pw.write("see  http://www.bath.ac.uk/~ensab/G_mod/Svlis/ \"\n")
    pw.write(" ]\n")
    pw.write(" title \"svLis\"\n")
    pw.write("}\n")
  }

  def write(m: Model) = {
    writeHeader()
    pw.write("Transform {\n")
    pw.write(" children [\n")
    pw.write("  NavigationInfo { headlight TRUE type \"EXAMINE\"}\n")
    pw.write("  Viewpoint { orientation 0 0 0  0  position 0 0 10  description \"Front\" }\n")
    pw.write("  Background { groundColor [ 0.3 0.2 0.1 ] skyColor [ 0.6 0.7 1.0 ] }\n")
    // val d = sqrt(b.diag_sq())
    val c = m.box.centroid // TODO b.centroid + SV_Z * d
    writeTranslatedChildren("  ", -c.x, -c.y, -c.z, () => {
      if (writeAxes) writeAxes("   ", m)
      m.walk(writeModel)
    })
    pw.write(" ]\n")
    pw.write("}\n")
  }
}

object ModelToVrml {
  def apply(m: Model, filename: String, writeAxes: Boolean = false, writeBoxes: Boolean = false, writeVoxels: Boolean = true, writeFaces: Boolean = true): ModelToVrml = {
    val file = new File(filename)
    val pw = new PrintWriter(file, "UTF-8")
    new ModelToVrml(pw, writeAxes, writeBoxes, writeVoxels, writeFaces)
  }

  def getSvlisVersion(): String = "TODO"

  def write(m: Model, filename: String, writeAxes: Boolean = false, writeBoxes: Boolean = false, writeVoxels: Boolean = true, writeFaces: Boolean = true): Unit = {
    val m2v = apply(m, filename, writeAxes, writeBoxes, writeVoxels, writeFaces)
    m2v.write(m)
    m2v.pw.close()
  }
}
