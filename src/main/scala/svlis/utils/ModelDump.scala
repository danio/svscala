package svlis
package utils

import lib._

object ModelDump {
  def dump(m: Model) = m.walk((m: Model, level: Int) => {
    val indent = " " * level
    println(s"${indent} ${m.box} ${m.set.contents}")
  })
}
