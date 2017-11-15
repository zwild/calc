package io.github.zwild

import scala.collection.mutable.HashMap

trait Env {
  val env = HashMap.empty[String, Double]

  val funEnv = HashMap(
    "ceil" -> Math.ceil _,
    "floor" -> Math.floor _,
    "exp" -> Math.exp _,
    "log" -> Math.log _,
    "sin" -> Math.sin _,
    "cos" -> Math.cos _,
    "tan" -> Math.tan _
  )
}
