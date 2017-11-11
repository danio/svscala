package svlis
package utils

object ModelDump {
  def dump_r(m: Model, level: Int = 0): Unit = {
    val indent = " " * level
    println(s"${indent} ${m.box} ${m.set.contents}")
    m match {
      case LeafModel(_, _, _) => ()
      case DividedModel(_, _, _, c1, c2) => {
        dump_r(c1, level + 1)
        dump_r(c2, level + 1)
      }
    }
  }
  def dump(m: Model) = dump_r(m, 0)
}
