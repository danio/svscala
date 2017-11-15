package svlis
package utils

import java.io._
import lib._

// VRML export logic from polygon.cxx lines 1861-2116

class ModelToVrml(val pw: PrintWriter, val writeBoxes: Boolean = false, val writeVoxels: Boolean = true, val writeFaces: Boolean = true) {
  def writeModel(m: Model, level: Int) = {
    m match {
      case LeafModel(_, _, _) => writeBox(m)
      case DividedModel(_, _, _, c1, c2) => ()
    }
  }

  def writeBox(m: Model) = {
    if (m.set.contents != Contents.Nothing) {
      val sx = m.box.xi.hi - m.box.xi.lo
      val sy = m.box.yi.hi - m.box.yi.lo
      val sz = m.box.zi.hi - m.box.zi.lo
      // VRML boxes are centred about the origin, so they
      // need to be translated by half their size and also their box minimum
      val tx = sx / 2 + m.box.xi.lo
      val ty = sy / 2 + m.box.yi.lo
      val tz = sz / 2 + m.box.zi.lo
      val b = m.box
      pw.write(s"    Transform {\n     translation $tx $ty $tz\n     children [\n")
      pw.write("    Shape {\n")
      pw.write("     geometry Box {\n")
      pw.write(s"      size $sx $sy $sz\n")
      pw.write("     }\n")
      pw.write("     appearance Appearance { \n")
      pw.write("       material Material { ")
      if (m.set.contents != Contents.Everything) {
        pw.write("transparency 0.2 ")
      }
      // TODO pw.write(" diffuseColor 1.0 1.0 0.0")
      pw.write("}\n")
      pw.write("     }\n")
      pw.write("    }\n")
      pw.write("    ] }\n")
    }
  }

  def write(m: Model): Unit = {
    val b = m.box
    val c = b.centroid() // TODO b.centroid + SV_Z * d

    pw.write("#VRML V2.0 utf8\n\n")
    pw.write("WorldInfo {\n")
    pw.write(" info [\n")
    pw.write("  \"Created by svLis version " + ModelToVrml.getSvlisVersion() + " - ")
    pw.write("see  http://www.bath.ac.uk/~ensab/G_mod/Svlis/ \"\n")
    pw.write(" ]\n")
    pw.write(" title \"svLis\"\n")
    pw.write("}\n")
    pw.write("Transform {\n")
    pw.write(" children [\n")
    pw.write("  NavigationInfo { headlight TRUE type \"EXAMINE\"}\n")
    pw.write("  Viewpoint { orientation 0 0 0  0  position 0 0 10  description \"Front\" }\n")
    pw.write("  Background {\n")
    pw.write("   groundColor  [ 0.3 0.2 0.1 ]\n")
    pw.write("   skyColor  [ 0.6 0.7 1.0 ]\n")
    pw.write("  }\n")
    pw.write("  Transform {\n")
    pw.write("   translation " + -c.x + ' ' + -c.y + ' ' + -c.z + "\n")
    pw.write("   children [\n")

    m.walk(writeModel)

    pw.write("   ]\n")
    pw.write("  }\n")
    pw.write(" ]\n")
    pw.write("}\n")
  }
}

object ModelToVrml {
  def apply(m: Model, filename: String, writeBoxes: Boolean = false, writeVoxels: Boolean = true, writeFaces: Boolean = true): ModelToVrml = {
    val file = new File(filename)
    val pw = new PrintWriter(file, "UTF-8")
    new ModelToVrml(pw, writeBoxes, writeVoxels, writeFaces)
  }

  def getSvlisVersion(): String = "TODO"

  def write(m: Model, filename: String, writeBoxes: Boolean = false, writeVoxels: Boolean = true, writeFaces: Boolean = true): Unit = {
    val m2v = apply(m, filename, writeBoxes, writeVoxels, writeFaces)
    m2v.write(m)
    m2v.pw.close()
  }
}
