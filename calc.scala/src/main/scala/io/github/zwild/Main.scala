package io.github.zwild

object Main {
  def main(arg: Array[String]): Unit = {
    val result = CalcParsers("10.0 * 4 + (1.4 * 7 - 4)")
    println(result)
  }
}
